use std::ops::Range;

use bitfield_struct::bitenum;
use register_macros::gba_register;
use tinyvec::ArrayVec;

use crate::Rgb5;
use crate::ppu::Ppu;
use crate::ppu::common::*;
use crate::ppu::core::Memory;
use crate::ppu::registers::BgMode;
use crate::ppu::registers::ObjectMapType;
use PaletteType::*;

/// sprites tiles occupy the last 32kb of vram
const SPRITE_VRAM_CHUNK: Range<usize> = 0x0001_0000..0x0001_8000;
/// each oam is 8 bytes in size
const OAM_ENTRY_SIZE: usize = 8;
/// each matrix is located in a 32 byte chunk in oam
const AFFINE_MATRIX_SIZE: usize = 32;

pub struct SpriteFetcher {
    /// Holds sprites that are enabled
    /// and vertically intersecting with current scanline.
    /// Sprites are stored sorted
    /// based on their priority while respecting the original relative
    /// order for sprites with equal priority.
    sprite_buffer: ArrayVec<[u8; MAX_SPRITES]>,
    pixel_counter_x: u8,
    y_coord: u8,
    dimension: ObjectMapType,
    background_mode: BgMode,
}

impl SpriteFetcher {
    pub fn new(ppu: &Ppu) -> Self {
        let mut sprite_fetcher = Self {
            sprite_buffer: ArrayVec::new(),
            pixel_counter_x: 0,
            y_coord: ppu.registers.v_counter.scanline_count(),
            dimension: ppu.registers.lcd_control.obj_vram_mapping(),
            background_mode: ppu.registers.lcd_control.bg_mode(),
        };

        if !ppu.registers.lcd_control.obj_enable() {
            return sprite_fetcher;
        }

        let (oam, _) = ppu.mem.oam.as_chunks::<OAM_ENTRY_SIZE>();
        let y_coord = ppu.registers.v_counter.scanline_count();

        for (oam_number, oam_entry) in oam.iter().map(OamEntry::new).enumerate() {
            let Vector2 { y: height, .. } = get_sprite_size(&oam_entry);
            let y_start = oam_entry.attribute0.y_coord();

            let fine_y = y_coord.wrapping_sub(y_start);
            let sprite_present = fine_y < height;
            let enabled = !matches!(oam_entry.attribute0.object_mode(), ObjectMode::Disabled);

            if sprite_present && enabled {
                sprite_fetcher.sprite_buffer.push(oam_number as u8);
            }
        }

        sprite_fetcher.sprite_buffer.sort_by_key(|sprite_index| {
            let oam_entry = OamEntry::new(&oam[usize::from(*sprite_index)]);
            oam_entry.attribute2.priority()
        });

        sprite_fetcher
    }
}

/// Fetches sprite pixel. If no sprite is present then a transparent pixel type is returned instead.
pub fn fetch_sprite_pixel(sprite_fetcher: &mut SpriteFetcher, mem: &Memory) -> Option<OutputPixel> {
    let (oam, _) = mem.oam.as_chunks::<OAM_ENTRY_SIZE>();
    let x_coord = sprite_fetcher.pixel_counter_x;
    let y_coord = sprite_fetcher.y_coord;
    let pixel_coords = Vector2::new(x_coord, y_coord);
    let dimension = sprite_fetcher.dimension;
    let bg_mode = sprite_fetcher.background_mode;

    sprite_fetcher.pixel_counter_x += 1;

    sprite_fetcher
        .sprite_buffer
        .iter()
        .copied()
        .filter_map(|sprite_index| {
            let oam_entry = OamEntry::new(&oam[usize::from(sprite_index)]);

            let x_start = oam_entry.attribute1.x_coord();
            let Vector2 { x: width, .. } = get_sprite_size(&oam_entry);

            let fine_x = wrapping_sub_512(u16::from(x_coord), x_start);
            let sprite_present = fine_x < u16::from(width);

            if sprite_present {
                Some(oam_entry)
            } else {
                None
            }
        })
        .map(|oam_entry| fetch_pixel(&oam_entry, mem, pixel_coords, dimension, bg_mode))
        .find(|pixel_type| pixel_type.is_some())
        .flatten()
}

/// Calculate coordinates to fetch pixel from in texture space (xy coordinated within a sprite).
/// Returns None for affine sprites where transformation causes pixel to fall outside of the sprite bounds.
/// # Arguments
/// * `oam_entry` - attributes of the sprite
/// * `q` - vector pointing to pixel to fetch in screen space
/// * `mem` - memory components to fetch from
fn get_pixel_coord(oam_entry: &OamEntry, q: Vector2<u8>, mem: &Memory) -> Option<Vector2<u16>> {
    let x_start = oam_entry.attribute1.x_coord();
    let y_start = oam_entry.attribute0.y_coord();
    let sprite_size = get_sprite_size(oam_entry);
    let is_affine = oam_entry.attribute0.is_affine();

    if !is_affine {
        // x and y pixel coordinates within a sprite
        let sprite_fine_x = if oam_entry.attribute1.horizontal_flip() {
            u16::from(sprite_size.x) - wrapping_sub_512(u16::from(q.x), x_start)
        } else {
            wrapping_sub_512(u16::from(q.x), x_start)
        };

        let sprite_fine_y = if oam_entry.attribute1.vertical_flip() {
            u16::from(sprite_size.y - q.y.wrapping_sub(y_start))
        } else {
            u16::from(q.y.wrapping_sub(y_start))
        };

        return Some(Vector2::new(sprite_fine_x, sprite_fine_y));
    }

    let (matrices, _) = mem.oam.as_chunks::<AFFINE_MATRIX_SIZE>();
    let AffineEntry { pa, pb, pc, pd } =
        AffineEntry::new(&matrices[oam_entry.attribute1.affine_index()]);

    // vector pointing to center of sprite (origin) in texture space
    let origin = Vector2::new(i32::from(sprite_size.x) / 2, i32::from(sprite_size.y) / 2);

    // vector pointing to pixel to fetch in texture space
    let pixel = {
        let x = wrapping_sub_512(u16::from(q.x), x_start);
        let y = u16::from(q.y.wrapping_sub(y_start));
        Vector2::new(x as i32, y as i32)
    };

    // Vector pointing from origin to the pixel to fetch in texture space
    let origin_to_pixel = pixel.sub(origin);

    let rotated_origin_to_pixel = Vector2 {
        x: (pa * origin_to_pixel.x + pb * origin_to_pixel.y) >> 8,
        y: (pc * origin_to_pixel.x + pd * origin_to_pixel.y) >> 8,
    };

    let pixel_coords = origin.add(rotated_origin_to_pixel);

    if pixel_coords.x.is_negative()
        || pixel_coords.y.is_negative()
        || pixel_coords.x >= i32::from(sprite_size.x)
        || pixel_coords.y >= i32::from(sprite_size.y)
    {
        None
    } else {
        Some(Vector2::new(pixel_coords.x as u16, pixel_coords.y as u16))
    }
}

/// Fetches the pixel of corresponding sprite at pixel coords in screen space.
/// If pixel is transparent, returns None, otherwise Some contains the output
/// color.
fn fetch_pixel(
    oam_entry: &OamEntry,
    mem: &Memory,
    pixel_coords: Vector2<u8>,
    dimension: ObjectMapType,
    bg_mode: BgMode,
) -> Option<OutputPixel> {
    let sprite_fine = get_pixel_coord(&oam_entry, pixel_coords, mem)?;

    // x and y coordinate within a 8x8 tile
    let fine_x = usize::from(sprite_fine.x % 8);
    let fine_y = usize::from(sprite_fine.y % 8);

    // x and y tile coord inside sprite
    let tile_x = sprite_fine.x / u16::from(TILE_PIXEL_SIZE);
    let tile_y = sprite_fine.y / u16::from(TILE_PIXEL_SIZE);

    let width_tiles = u16::from(get_sprite_tile_size(oam_entry).x);
    let tile_index_base = oam_entry.attribute2.tile_index();
    let tile_index = match dimension {
        // treats tiles as if they are arranged in a 32 by 32 tile matrix
        ObjectMapType::D2 => tile_index_base + usize::from(tile_y * 32 + tile_x),
        ObjectMapType::D1 => tile_index_base + usize::from(tile_y * width_tiles + tile_x),
    };

    // The first 512 tiles cannot be displayed when rendering bitmap backgrounds.
    // This is because bitmaps use more than 64kb of memory and will partially ocuppy
    // the first 16kb portion of vram dedicated to sprites.
    if bg_mode.is_bitmap() && tile_index < 512 {
        return None;
    }

    match oam_entry.attribute0.palette_type() {
        ColorDepth4Bit => {
            let (tiles, _) = mem.vram[SPRITE_VRAM_CHUNK].as_chunks::<S_TILE_SIZE>();
            let (s_tile, _) = tiles[tile_index % tiles.len()].as_chunks::<S_TILE_ROW_SIZE>();

            let sprite_row = s_tile[fine_y];
            let pixel_pair = sprite_row[fine_x / 2];
            let shift = 4 * (fine_x & 1);
            let color_index = usize::from(pixel_pair & (0xF << shift)) >> shift;

            if color_index != 0 {
                let (palettes, _) = mem.palette_ram[OBJ_PALETTE].as_chunks::<PALETTE_SIZE_4BPP>();
                let palette_index = oam_entry.attribute2.palette_number();
                let (color_palette, _) = palettes[palette_index].as_chunks::<RGB5_SIZE>();
                let color_bits = u16::from_le_bytes(color_palette[color_index]);

                Some(OutputPixel {
                    color: Rgb5::from_bits(color_bits),
                    priority: oam_entry.attribute2.priority(),
                })
            } else {
                None
            }
        }
        ColorDepth8Bit => {
            let (tiles, _) = mem.vram[SPRITE_VRAM_CHUNK].as_chunks::<D_TILE_SIZE>();
            let (d_tile, _) = tiles[tile_index % tiles.len()].as_chunks::<D_TILE_ROW_SIZE>();

            let sprite_row = d_tile[fine_y];
            let color_index = usize::from(sprite_row[fine_x]);

            if color_index != 0 {
                let (color_palette, _) = mem.palette_ram[OBJ_PALETTE].as_chunks::<RGB5_SIZE>();
                let color_bits = u16::from_le_bytes(color_palette[color_index]);

                Some(OutputPixel {
                    color: Rgb5::from_bits(color_bits),
                    priority: oam_entry.attribute2.priority(),
                })
            } else {
                None
            }
        }
    }
}

struct OamEntry {
    attribute0: Attribute0,
    attribute1: Attribute1,
    attribute2: Attribute2,
}

impl OamEntry {
    fn new(entry: &[u8; 8]) -> Self {
        let attribute0 = Attribute0::from_bits(u16::from_le_bytes([entry[0], entry[1]]));
        let attribute1 = Attribute1::from_bits(u16::from_le_bytes([entry[2], entry[3]]));
        let attribute2 = Attribute2::from_bits(u16::from_le_bytes([entry[4], entry[5]]));

        Self {
            attribute0,
            attribute1,
            attribute2,
        }
    }
}

/// Affine transformation matrix.
/// |pa, pb|
/// |pc, pd|
struct AffineEntry {
    pa: i32,
    pb: i32,
    pc: i32,
    pd: i32,
}

impl AffineEntry {
    fn new(entry: &[u8; 32]) -> Self {
        const SHORT: usize = size_of::<u16>();

        let pa = i16::from_le_bytes([entry[SHORT * 3], entry[SHORT * 3 + 1]]).into();
        let pb = i16::from_le_bytes([entry[SHORT * 7], entry[SHORT * 7 + 1]]).into();
        let pc = i16::from_le_bytes([entry[SHORT * 11], entry[SHORT * 11 + 1]]).into();
        let pd = i16::from_le_bytes([entry[SHORT * 15], entry[SHORT * 15 + 1]]).into();

        Self { pa, pb, pc, pd }
    }
}

#[gba_register(u16)]
struct Attribute0 {
    y_coord: u8,

    #[bits(2, default = ObjectMode::Normal)]
    object_mode: ObjectMode,

    #[bits(2, default = GfxMode::Normal)]
    gfx_mode: GfxMode,

    enable_mosaic: bool,

    #[bits(1, default = PaletteType::ColorDepth4Bit )]
    palette_type: PaletteType,

    #[bits(2, default = Shape::Square)]
    shape: Shape,
}

impl Attribute0 {
    #[rustfmt::skip]
    fn is_affine(&self) -> bool {
        matches!(self.object_mode(), ObjectMode::Affine | ObjectMode::AffineDouble)
    }
}

#[gba_register(u16)]
struct Attribute1 {
    #[bits(9)]
    x_coord: u16,

    #[bits(5)]
    affine_index_or_flip: u8,

    #[bits(2)]
    sprite_size: usize,
}

impl Attribute1 {
    fn affine_index(&self) -> usize {
        usize::from(self.affine_index_or_flip())
    }

    /// ignored when sprite is affine
    fn horizontal_flip(&self) -> bool {
        (self.affine_index_or_flip() >> 3) & 1 == 1
    }

    /// ignored when sprite is affine
    fn vertical_flip(&self) -> bool {
        (self.affine_index_or_flip() >> 4) & 1 == 1
    }
}

#[gba_register(u16)]
struct Attribute2 {
    #[bits(10)]
    tile_index: usize,

    #[bits(2)]
    priority: u8,

    #[bits(4)]
    palette_number: usize,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
enum ObjectMode {
    #[fallback]
    Normal = 0,
    Affine,
    Disabled,
    AffineDouble,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
enum GfxMode {
    #[fallback]
    Normal = 0,
    AlphaBlend,
    Window,
    Forbidden,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
enum Shape {
    #[fallback]
    Square = 0,
    Wide,
    Tall,
}

static SPRITE_SIZE_TABLE: [[(u8, u8); 4]; 3] = [
    [(8, 8), (16, 16), (32, 32), (64, 64)],
    [(16, 8), (32, 8), (32, 16), (64, 32)],
    [(8, 16), (8, 32), (16, 32), (32, 64)],
];

fn get_sprite_size(oam_entry: &OamEntry) -> Vector2<u8> {
    let size = oam_entry.attribute1.sprite_size();
    let is_double = matches!(oam_entry.attribute0.object_mode(), ObjectMode::AffineDouble);

    let mut sprite_size = match oam_entry.attribute0.shape() {
        Shape::Square => SPRITE_SIZE_TABLE[0][size],
        Shape::Wide => SPRITE_SIZE_TABLE[1][size],
        Shape::Tall => SPRITE_SIZE_TABLE[2][size],
    };

    if is_double {
        sprite_size.0 *= 2;
        sprite_size.1 *= 2;
    }

    Vector2::from(sprite_size)
}

fn get_sprite_tile_size(oam_entry: &OamEntry) -> Vector2<u8> {
    let mut sprite_size = get_sprite_size(oam_entry);
    sprite_size.x /= TILE_PIXEL_SIZE;
    sprite_size.y /= TILE_PIXEL_SIZE;
    sprite_size
}

fn wrapping_sub_512(op1: u16, op2: u16) -> u16 {
    let diff = op1.wrapping_sub(op2);
    diff % 512
}
