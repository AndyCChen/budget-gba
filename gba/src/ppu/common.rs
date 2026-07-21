use crate::Rgb5;
use bitfield_struct::bitenum;
use num_traits::PrimInt;
use std::ops::Range;

/// Colors are 16 bit rgb values (2 bytes)
pub const RGB5_SIZE: usize = 2;

/// Size in bytes for a single color palette for 4bpp tiles.
pub const PALETTE_SIZE_4BPP: usize = 32;
/// Palette ram is divide into two 512 byte regions, one for
/// background tiles and the other for sprites
pub const PALETTE_REGION_SIZE: usize = 512;

/// bg palette uses the first 512 bytes of palette ram
pub const BG_PALETTE: Range<usize> = 0..PALETTE_REGION_SIZE;
/// obj palette uses the second 512 bytes of palette ram
pub const OBJ_PALETTE: Range<usize> = 512..(PALETTE_REGION_SIZE * 2);

/// Tiles are grouped into 16kb blocks
pub const CHAR_BLOCK_SIZE: usize = 16 * 1024;
/// Screen blocks are grouped into 2kb blocks
pub const SCREEN_BLOCK_SIZE: usize = 2 * 1024;

/// Size of tiles in 4bpp format, 32 bytes
pub const S_TILE_SIZE: usize = 32;
/// Size of tiles in 8bpp format, 64 bytes
pub const D_TILE_SIZE: usize = 64;

/// each row is 4 bytes
pub const S_TILE_ROW_SIZE: usize = 4;
/// each row is 8 bytes
pub const D_TILE_ROW_SIZE: usize = 8;

/// screen block is 32 tiles wide
pub const SCREEN_BLOCK_WIDTH: usize = 32;
/// screen block is 32 tiles high
pub const SCREEN_BLOCK_HEIGHT: usize = 32;

/// screen entry is 2 bytes
pub const SCREEN_ENTRY_SIZE: usize = 2;

/// In the best case, up to 128 objects can be displayed on a scanline
pub const MAX_SPRITES: usize = 128;

#[bitenum]
#[repr(u8)]
#[derive(Debug)]
pub enum PaletteType {
    /// 16 colors / 16 palettes
    #[fallback]
    ColorDepth4Bit = 0,
    /// 256 colors / 1 palette
    ColorDepth8Bit,
}

#[derive(Default, Clone)]
pub struct OutputPixel {
    pub color: Rgb5,
    pub priority: u8,
}

#[derive(Debug, Copy, Clone)]
pub struct Vector2<T: PrimInt> {
    pub x: T,
    pub y: T,
}

impl<T: PrimInt> Vector2<T> {
    pub fn new(x: T, y: T) -> Self {
        Self { x, y }
    }

    pub fn sub(self, rhs: Vector2<T>) -> Self {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }

    pub fn add(self, rhs: Vector2<T>) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl<T: PrimInt> From<(T, T)> for Vector2<T> {
    fn from(value: (T, T)) -> Self {
        Self {
            x: value.0,
            y: value.1,
        }
    }
}
