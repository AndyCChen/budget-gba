use bitfield_struct::bitenum;
use register_macros::gba_register;

use crate::ppu::common::PaletteType;

static SPRITE_SIZE_TABLE: [[(usize, usize); 4]; 3] = [
    [(8, 8), (16, 16), (32, 32), (64, 64)],
    [(16, 8), (32, 8), (32, 16), (64, 32)],
    [(8, 16), (8, 32), (16, 32), (32, 64)],
];

#[gba_register(u16)]
pub struct Attribute0 {
    pub y_coord: u8,

    #[bits(2, default = ObjectMode::Normal)]
    pub object_mode: ObjectMode,

    #[bits(2, default = GfxMode::Normal)]
    pub gfx_mode: GfxMode,

    pub enable_mosaic: bool,

    #[bits(1, default = PaletteType::  ColorDepth4Bit )]
    pub palette_type: PaletteType,

    #[bits(2, default = Shape::Square)]
    pub shape: Shape,
}

impl Attribute0 {
    #[rustfmt::skip]
    pub fn is_affine(&self) -> bool {
        matches!(self.object_mode(), ObjectMode::Affine | ObjectMode::AffineDouble)
    }
}

#[gba_register(u16)]
pub struct Attribute1 {
    #[bits(9)]
    pub x_coord: u16,

    #[bits(5)]
    pub affine_index_or_flip: u8,

    #[bits(2)]
    pub sprite_size: u8,
}

impl Attribute1 {
    pub fn affine_index(&self) -> u8 {
        self.affine_index_or_flip()
    }

    pub fn horizontal_flip(&self) -> bool {
        (self.affine_index_or_flip() >> 3) & 1 == 1
    }

    pub fn vertical_flip(&self) -> bool {
        (self.affine_index_or_flip() >> 4) & 1 == 1
    }
}

#[gba_register(u16)]
pub struct Attribute2 {
    #[bits(10)]
    pub tile_index: u16,

    #[bits(2)]
    pub priority: u8,

    #[bits(4)]
    pub palette_number: usize,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
pub enum ObjectMode {
    #[fallback]
    Normal = 0,
    Affine,
    Disabled,
    AffineDouble,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
pub enum GfxMode {
    #[fallback]
    Normal = 0,
    AlphaBlend,
    Window,
    Forbidden,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
pub enum Shape {
    #[fallback]
    Square = 0,
    Wide,
    Tall,
}
