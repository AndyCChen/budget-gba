use bitfield_struct::bitfield;
use std::array;

use crate::Rgb5;
use crate::ppu::common::*;
use crate::ppu::core::Memory;
use crate::ppu::registers::{BgControl, BgScroll};

/// Fetch pixel from background layer. If pixel is transparent, return None,
/// else the opaque color is wrapped in Some.
pub fn fetch_pixel(mem: &Memory, fetch_type: &mut FetchType) -> Option<OutputPixel> {
    match fetch_type {
        FetchType::Fetch4bpp(background_4bpp) => fetch_pixel_4bpp(background_4bpp, mem),
        FetchType::Fetch8bpp(background_8bpp) => fetch_pixel_8bpp(background_8bpp, mem),
    }
}

fn fetch_pixel_4bpp(bg_4bpp: &mut BackGround4bpp, mem: &Memory) -> Option<OutputPixel> {
    if bg_4bpp.pixel_x_counter.is_multiple_of(8) {
        fetch_tile_4bpp(bg_4bpp, mem);
    }

    bg_4bpp.pixel_x_counter += 1;

    let fine_x = bg_4bpp.bg.scroll_x.offset() & 7;
    let pixel_select = S_TILE_ROW_SIZE * (7 - fine_x);

    // index within a 16 color palette
    let color_index = (bg_4bpp.pixel_shifter.output() >> pixel_select) & 0xF;
    // index to select one of the 16 palettes
    let palette_selection = (bg_4bpp.palette_shifter.output() >> pixel_select) & 0xF;

    bg_4bpp.pixel_shifter = Shifter4Bpp::from_bits(bg_4bpp.pixel_shifter.into_bits() << 4);
    bg_4bpp.palette_shifter = Shifter4Bpp::from_bits(bg_4bpp.palette_shifter.into_bits() << 4);

    if color_index == 0 {
        None
    } else {
        let (palettes, _) = mem.palette_ram[BG_PALETTE].as_chunks::<PALETTE_SIZE_4BPP>();
        let (color_palette, _) = palettes[palette_selection as usize].as_chunks::<RGB5_SIZE>();
        let color_bytes = u16::from_le_bytes(color_palette[color_index as usize]);
        let priority = bg_4bpp.bg.bg_control.bg_priority();

        Some(OutputPixel {
            color: Rgb5::from_u16(color_bytes),
            priority,
        })
    }
}

fn fetch_tile_4bpp(bg_4bpp: &mut BackGround4bpp, mem: &Memory) {
    let char_base = usize::from(bg_4bpp.bg.bg_control.char_base_block()) * CHAR_BLOCK_SIZE;
    let screen_base = usize::from(bg_4bpp.bg.bg_control.screen_base_block()) * SCREEN_BLOCK_SIZE;
    let layout = bg_4bpp.bg.bg_control.screen_size();

    let (screen_blocks, _) = mem.vram
        [screen_base..screen_base + (SCREEN_BLOCK_SIZE * layout.get_block_count())]
        .as_chunks::<SCREEN_BLOCK_SIZE>();

    let (char_tiles, _) = mem.vram[char_base..].as_chunks::<S_TILE_SIZE>();
    let (layout_width, layout_height) = layout.layout_tile_size();

    let tile_x = usize::from(bg_4bpp.tile_x) % usize::from(layout_width);
    let tile_y = usize::from((bg_4bpp.screen_y / 8) % u16::from(layout_height));

    let screen_block_index = (tile_y / SCREEN_BLOCK_WIDTH)
        * (usize::from(layout_width) / SCREEN_BLOCK_WIDTH)
        + (tile_x / SCREEN_BLOCK_HEIGHT);

    let inner_screen_block_index =
        (tile_y % SCREEN_BLOCK_WIDTH) * SCREEN_BLOCK_WIDTH + (tile_x % SCREEN_BLOCK_HEIGHT);

    bg_4bpp.tile_x += 1;

    let (screen_block, _) = screen_blocks[screen_block_index].as_chunks::<SCREEN_ENTRY_SIZE>();
    let screen_entry_bits = u16::from_le_bytes(screen_block[inner_screen_block_index]);
    let screen_entry = TextScreenEntry::from_bits(screen_entry_bits);

    let (char_entry, _) = char_tiles[screen_entry.tile_number()].as_chunks::<S_TILE_ROW_SIZE>();

    let fine_y = usize::from(if screen_entry.vertical_flip() {
        7 - (bg_4bpp.screen_y % 8)
    } else {
        bg_4bpp.screen_y % 8
    });

    // use fine_y to select a 4 byte pixel row
    let mut pixel_row = if screen_entry.horizontal_flip() {
        let mut flipped = char_entry[fine_y];
        flipped.reverse();
        flipped
    } else {
        char_entry[fine_y]
    };

    // lo nibble is the left pixel while the hi nibble is the right pixel,
    // this complicates things when outputing pixels so we swap the nibbles via left rotate.
    for byte in pixel_row.iter_mut() {
        *byte = byte.rotate_left(4);
    }

    let palette_number = screen_entry.palette_number() as u8;
    let palette = array::from_fn(|_| (palette_number << 4) | palette_number);

    bg_4bpp
        .palette_shifter
        .set_input(u32::from_be_bytes(palette));
    bg_4bpp
        .pixel_shifter
        .set_input(u32::from_be_bytes(pixel_row));
}

fn fetch_pixel_8bpp(bg_8bpp: &mut BackGround8bpp, mem: &Memory) -> Option<OutputPixel> {
    if bg_8bpp.pixel_x_counter.is_multiple_of(8) {
        fetch_tile_8bpp(bg_8bpp, mem);
    }

    bg_8bpp.pixel_x_counter += 1;

    let fine_x = bg_8bpp.bg.scroll_x.offset() & 7;
    let pixel_select = D_TILE_ROW_SIZE * (7 - fine_x);

    // index within a 256 color palette
    let color_index = (bg_8bpp.pixel_shifter.output() >> pixel_select) & 0xFF;

    bg_8bpp.pixel_shifter = Shifter8Bpp::from_bits(bg_8bpp.pixel_shifter.into_bits() << 8);

    if color_index == 0 {
        None
    } else {
        // palette for 8bpp mode is one big palette with 256 colors
        let (palette, _) = mem.palette_ram[BG_PALETTE].as_chunks::<RGB5_SIZE>();
        let color_bytes = u16::from_le_bytes(palette[color_index as usize]);
        let priority = bg_8bpp.bg.bg_control.bg_priority();

        Some(OutputPixel {
            color: Rgb5::from_u16(color_bytes),
            priority,
        })
    }
}

fn fetch_tile_8bpp(bg_8bpp: &mut BackGround8bpp, mem: &Memory) {
    let char_base = usize::from(bg_8bpp.bg.bg_control.char_base_block()) * CHAR_BLOCK_SIZE;
    let screen_base = usize::from(bg_8bpp.bg.bg_control.screen_base_block()) * SCREEN_BLOCK_SIZE;
    let layout = bg_8bpp.bg.bg_control.screen_size();

    let (screen_blocks, _) = mem.vram
        [screen_base..screen_base + (SCREEN_BLOCK_SIZE * layout.get_block_count())]
        .as_chunks::<SCREEN_BLOCK_SIZE>();

    let (char_tiles, _) = mem.vram[char_base..].as_chunks::<D_TILE_SIZE>();
    let (layout_width, layout_height) = layout.layout_tile_size();

    let tile_x = usize::from(bg_8bpp.tile_x) % usize::from(layout_width);
    let tile_y = usize::from((bg_8bpp.screen_y / 8) % u16::from(layout_height));

    let screen_block_index = (tile_y / SCREEN_BLOCK_WIDTH)
        * (usize::from(layout_width) / SCREEN_BLOCK_WIDTH)
        + (tile_x / SCREEN_BLOCK_HEIGHT);

    let inner_screen_block_index =
        (tile_y % SCREEN_BLOCK_WIDTH) * SCREEN_BLOCK_WIDTH + (tile_x % SCREEN_BLOCK_HEIGHT);

    bg_8bpp.tile_x += 1;

    let (screen_block, _) = screen_blocks[screen_block_index].as_chunks::<SCREEN_ENTRY_SIZE>();
    let screen_entry_bits = u16::from_le_bytes(screen_block[inner_screen_block_index]);
    let screen_entry = TextScreenEntry::from_bits(screen_entry_bits);

    let (char_entry, _) = char_tiles[screen_entry.tile_number()].as_chunks::<D_TILE_ROW_SIZE>();

    let fine_y = usize::from(if screen_entry.vertical_flip() {
        7 - (bg_8bpp.screen_y % 8)
    } else {
        bg_8bpp.screen_y % 8
    });

    // use fine_y to select a 4 byte pixel row
    let pixel_row = if screen_entry.horizontal_flip() {
        let mut flipped = char_entry[fine_y];
        flipped.reverse();
        flipped
    } else {
        char_entry[fine_y]
    };

    bg_8bpp
        .pixel_shifter
        .set_input(u64::from_be_bytes(pixel_row));
}

#[derive(Default)]
pub struct Background {
    pub bg_control: BgControl,
    pub scroll_x: BgScroll,
    pub scroll_y: BgScroll,
}

#[derive(Default)]
pub struct BackGround4bpp {
    tile_x: u8,
    screen_y: u16,
    bg: Background,
    pixel_shifter: Shifter4Bpp,
    palette_shifter: Shifter4Bpp,
    pixel_x_counter: u8,
}

impl BackGround4bpp {
    pub fn new(mem: &Memory, background: Background, scanline_y: usize) -> Self {
        let mut out = Self {
            tile_x: (background.scroll_x.offset() / 8) as u8,
            screen_y: (scanline_y + background.scroll_y.offset()) as u16,
            bg: background,
            pixel_shifter: Shifter4Bpp::default(),
            palette_shifter: Shifter4Bpp::default(),
            pixel_x_counter: 0,
        };

        for _ in 0..8 {
            fetch_pixel_4bpp(&mut out, mem);
        }

        out
    }
}

pub struct BackGround8bpp {
    tile_x: u8,
    screen_y: u16,
    bg: Background,
    pixel_shifter: Shifter8Bpp,
    pixel_x_counter: u8,
}

impl BackGround8bpp {
    pub fn new(mem: &Memory, background: Background, scanline_y: usize) -> Self {
        let mut out = Self {
            tile_x: (background.scroll_x.offset() / 8) as u8,
            screen_y: (scanline_y + background.scroll_y.offset()) as u16,
            bg: background,
            pixel_shifter: Shifter8Bpp::default(),
            pixel_x_counter: 0,
        };

        for _ in 0..8 {
            fetch_pixel_8bpp(&mut out, mem);
        }

        out
    }
}

#[bitfield(u64)]
struct Shifter4Bpp {
    input: u32,
    output: u32,
}

#[bitfield(u128)]
struct Shifter8Bpp {
    input: u64,
    output: u64,
}

#[bitfield(u16)]
struct TextScreenEntry {
    #[bits(10)]
    tile_number: usize,

    horizontal_flip: bool,
    vertical_flip: bool,

    #[bits(4)]
    palette_number: usize,
}

pub enum FetchType {
    Fetch4bpp(BackGround4bpp),
    Fetch8bpp(BackGround8bpp),
}

impl FetchType {
    pub fn priority(&self) -> u8 {
        match self {
            FetchType::Fetch4bpp(background4bpp) => background4bpp.bg.bg_control.bg_priority(),
            FetchType::Fetch8bpp(background8bpp) => background8bpp.bg.bg_control.bg_priority(),
        }
    }
}

impl Default for FetchType {
    fn default() -> Self {
        Self::Fetch4bpp(BackGround4bpp::default())
    }
}
