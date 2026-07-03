use std::ops::Range;

use bitfield_struct::bitenum;
use register_macros::gba_register;
use tinyvec::ArrayVec;

use crate::Rgb5;
use crate::ppu::Ppu;
use crate::ppu::common::*;
use crate::ppu::core::Memory;
use crate::ppu::registers::ObjectMapType;

const SPRITE_VRAM_CHUNK: Range<usize> = 0x0001_0000..0x0001_8000;
const OAM_ENTRY_SIZE: usize = 8;

pub fn fetch_sprite_pixel(sprite_fetcher: &mut SpriteFetcher, ppu: &Ppu) -> Option<PixelType> {
    let (oam, _) = ppu.mem.oam.as_chunks::<OAM_ENTRY_SIZE>();
    let x_coord = sprite_fetcher.pixel_counter_x;
    let y_coord = ppu.registers.v_counter.scanline_count();
    let dimension = ppu.registers.lcd_control.obj_vram_mapping();
    sprite_fetcher.pixel_counter_x += 1;

    let mut oam_iter = sprite_fetcher
        .sprite_buffer
        .iter()
        .copied()
        .filter_map(|sprite_number| {
            let oam_entry = OamEntry::new(&oam[usize::from(sprite_number)]);

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
        .map(|oam_entry| pixel_fetch_s_tile(&oam_entry, &ppu.mem, x_coord, y_coord, dimension));

    oam_iter.next()
}

fn pixel_fetch_s_tile(
    oam_entry: &OamEntry,
    mem: &Memory,
    x_coord: u8,
    y_coord: u8,
    dimension: ObjectMapType,
) -> PixelType {
    let x_start = oam_entry.attribute1.x_coord();

    // x and y pixel coordinates within a sprite
    let sprite_fine_x = wrapping_sub_512(u16::from(x_coord), x_start) as u8;
    let sprite_fine_y = y_coord.wrapping_sub(oam_entry.attribute0.y_coord());

    let (width, _) = get_sprite_tile_size(oam_entry);

    // x and y tile coord inside sprite
    let tile_x = sprite_fine_x / TILE_PIXEL_SIZE;
    let tile_y = sprite_fine_y / TILE_PIXEL_SIZE;

    let tile_index_base = oam_entry.attribute2.tile_index();

    let tile_index = match dimension {
        // treats tiles as if they are arranged in a 32 by 32 tile matrix
        ObjectMapType::D2 => tile_index_base + usize::from(tile_y * 32 + tile_x),
        ObjectMapType::D1 => tile_index_base + usize::from(tile_y * width + tile_x),
    };

    let (tiles, _) = mem.vram[SPRITE_VRAM_CHUNK].as_chunks::<S_TILE_SIZE>();
    let (s_tile, _) = tiles[tile_index].as_chunks::<S_TILE_ROW_SIZE>();

    // x and y coordinate within a 8x8 tile
    let fine_x = usize::from(sprite_fine_x % 8);
    let fine_y = usize::from(if oam_entry.attribute1.vertical_flip() {
        7 - (sprite_fine_y % 8)
    } else {
        sprite_fine_y % 8
    });

    let pixel_row = if oam_entry.attribute1.horizontal_flip() {
        let mut flipped = s_tile[fine_y];
        flipped.reverse();
        flipped
    } else {
        s_tile[fine_y]
    };

    let palette_index = oam_entry.attribute2.palette_number();
    let (palettes, _) = mem.palette_ram[OBJ_PALETTE].as_chunks::<PALETTE_SIZE_4BPP>();
    let (color_palette, _) = palettes[palette_index].as_chunks::<RGB5_SIZE>();

    let pixel_pair = pixel_row[fine_x / 2];
    let color_index = usize::from(pixel_pair & (0xF << (fine_x & 1)));
    let color_bytes = u16::from_le_bytes(color_palette[color_index]);

    let color = Rgb5::from_bits(color_bytes);
    let priority = oam_entry.attribute2.priority();

    if color_index == 0 {
        PixelType::Transparent
    } else {
        PixelType::Opaque { color, priority }
    }
}

pub struct SpriteFetcher {
    /// Holds sprites that are enabled
    /// and vertically intersect with current scanline
    /// being rendered.
    sprite_buffer: ArrayVec<[u8; MAX_SPRITES]>,
    pixel_counter_x: u8,
}

impl SpriteFetcher {
    pub fn new(ppu: &Ppu) -> Self {
        let mut sprite_fetcher = Self {
            sprite_buffer: ArrayVec::new(),
            pixel_counter_x: 0,
        };

        let (oam, _) = ppu.mem.oam.as_chunks::<OAM_ENTRY_SIZE>();
        let y_coord = ppu.registers.v_counter.scanline_count();

        for (oam_number, oam_entry) in oam.iter().map(|entry| OamEntry::new(entry)).enumerate() {
            let (_, height) = get_sprite_size(&oam_entry);
            let y_start = oam_entry.attribute0.y_coord();

            let fine_y = y_coord.wrapping_sub(y_start);
            let sprite_present = fine_y < height;
            let enabled = !matches!(oam_entry.attribute0.object_mode(), ObjectMode::Disabled);

            if sprite_present && enabled {
                sprite_fetcher.sprite_buffer.push(oam_number as u8);
            }
        }

        sprite_fetcher
    }
}

struct OamEntry {
    attribute0: Attribute0,
    attribute1: Attribute1,
    attribute2: Attribute2,
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

fn checked_add_512(op1: u16, op2: u16) -> Option<u16> {
    let sum = op1 + op2;
    if sum < 512 { Some(sum) } else { None }
}

fn wrapping_add_512(op1: u16, op2: u16) -> u16 {
    let sum = op1 + op2;
    sum % 512
}

fn wrapping_sub_512(op1: u16, op2: u16) -> u16 {
    let diff = op1.wrapping_sub(op2);
    diff % 512
}
