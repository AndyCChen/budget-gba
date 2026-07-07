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
            let (_, height) = get_sprite_size(&oam_entry);
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
            let (width, _) = get_sprite_size(&oam_entry);

            let fine_x = wrapping_sub_512(u16::from(x_coord), x_start);
            let sprite_present = fine_x < u16::from(width);

            if sprite_present {
                Some(oam_entry)
            } else {
                None
            }
        })
        .map(|oam_entry| fetch_pixel(&oam_entry, mem, x_coord, y_coord, dimension, bg_mode))
        .find(|pixel_type| pixel_type.is_some())
        .flatten()
}

/// Fetches the pixel of corresponding sprite at provided x and y coord.
/// If pixel is transparent, returns None, otherwise Some contains the output
/// color.
fn fetch_pixel(
    oam_entry: &OamEntry,
    mem: &Memory,
    x_coord: u8,
    y_coord: u8,
    dimension: ObjectMapType,
    bg_mode: BgMode,
) -> Option<OutputPixel> {
    let x_start = oam_entry.attribute1.x_coord();
    let (width_pixel, height_pixel) = get_sprite_size(oam_entry);

    // x and y pixel coordinates within a sprite
    let sprite_fine_x = if oam_entry.attribute1.horizontal_flip() {
        u16::from(width_pixel) - wrapping_sub_512(u16::from(x_coord), x_start)
    } else {
        wrapping_sub_512(u16::from(x_coord), x_start)
    };

    let sprite_fine_y = u16::from(if oam_entry.attribute1.vertical_flip() {
        height_pixel - y_coord.wrapping_sub(oam_entry.attribute0.y_coord())
    } else {
        y_coord.wrapping_sub(oam_entry.attribute0.y_coord())
    });

    // x and y coordinate within a 8x8 tile
    let fine_x = usize::from(sprite_fine_x % 8);
    let fine_y = usize::from(sprite_fine_y % 8);

    // x and y tile coord inside sprite
    let tile_x = sprite_fine_x / u16::from(TILE_PIXEL_SIZE);
    let tile_y = sprite_fine_y / u16::from(TILE_PIXEL_SIZE);

    let width_tiles = u16::from(get_sprite_tile_size(oam_entry).0);
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
    #[allow(unused)]
    padding: u16,
}

impl OamEntry {
    fn new(entry: &[u8; 8]) -> Self {
        let attribute0 = Attribute0::from_bits(u16::from_le_bytes([entry[0], entry[1]]));
        let attribute1 = Attribute1::from_bits(u16::from_le_bytes([entry[2], entry[3]]));
        let attribute2 = Attribute2::from_bits(u16::from_le_bytes([entry[4], entry[5]]));
        let padding = u16::from_le_bytes([entry[6], entry[7]]);

        Self {
            attribute0,
            attribute1,
            attribute2,
            padding,
        }
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
    fn _is_affine(&self) -> bool {
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
    fn _affine_index(&self) -> u8 {
        self.affine_index_or_flip()
    }

    fn horizontal_flip(&self) -> bool {
        (self.affine_index_or_flip() >> 3) & 1 == 1
    }

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

/// Retrive pixel dimensions of the sprite as a tuple (width, height)
fn get_sprite_size(oam_entry: &OamEntry) -> (u8, u8) {
    let size = oam_entry.attribute1.sprite_size();
    // TODO account for double sized affine sprites
    match oam_entry.attribute0.shape() {
        Shape::Square => SPRITE_SIZE_TABLE[0][size],
        Shape::Wide => SPRITE_SIZE_TABLE[1][size],
        Shape::Tall => SPRITE_SIZE_TABLE[2][size],
    }
}

/// Retrive tile dimensions of the sprite as a tuple (width, height)
fn get_sprite_tile_size(oam_entry: &OamEntry) -> (u8, u8) {
    let (width, height) = get_sprite_size(oam_entry);
    (width / TILE_PIXEL_SIZE, height / TILE_PIXEL_SIZE)
}

fn wrapping_sub_512(op1: u16, op2: u16) -> u16 {
    let diff = op1.wrapping_sub(op2);
    diff % 512
}
