use std::ops::Range;

/// Size in bytes for a single color palette for 4bpp tiles.
pub const PALETTE_SIZE_4BPP: usize = 32;
pub const PALETTE_REGION_SIZE: usize = 512;

/// bg palette uses the first 512 bytes of palette ram
pub const BG_PALETTE: Range<usize> = 0..PALETTE_REGION_SIZE;
/// obj palette usees the second 512 bytes of palette ram
pub const _OBJ_PALETTE: Range<usize> = 512..(PALETTE_REGION_SIZE * 2);

pub const CHAR_BLOCK_SIZE: usize = 16 * 1024;
pub const SCREEN_BLOCK_SIZE: usize = 2 * 1024;

/// Size of tiles in 4bpp format, 32 bytes big
pub const S_TILE_SIZE: usize = 32;
/// Size of tiles in 8bpp format, 64 bytes big
pub const _D_TILE_SIZE: usize = 64;

// screen block dimensions in tiles
pub const SCREEN_BLOCK_WIDTH: usize = 32;
pub const SCREEN_BLOCK_HEIGHT: usize = 32;
