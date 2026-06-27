use bitfield_struct::bitfield;
use std::array;

use crate::Rgb5;
use crate::ppu::common::*;
use crate::ppu::core::Memory;
use crate::ppu::registers::{BgControl, BgScroll};

pub enum FetchType {
    Fetch4bpp(BackGround4bpp),
    Fetch8bpp,
}

pub fn fetch_pixel(mem: &Memory, fetch_type: &mut FetchType) -> Rgb5 {
    match fetch_type {
        FetchType::Fetch4bpp(background_4bpp) => fetch_pixel_4bpp(background_4bpp, mem),
        FetchType::Fetch8bpp => todo!(),
    }
}

fn fetch_tile_4bpp(bg_4bpp: &mut BackGround4bpp, mem: &Memory) {
    let char_base = usize::from(bg_4bpp.background.bg_control.char_base_block()) * CHAR_BLOCK_SIZE;
    let screen_base =
        usize::from(bg_4bpp.background.bg_control.screen_base_block()) * SCREEN_BLOCK_SIZE;
    let layout = bg_4bpp.background.bg_control.screen_size();

    let (screen_blocks, _) = mem.vram
        [screen_base..screen_base + (SCREEN_BLOCK_SIZE * layout.get_block_count())]
        .as_chunks::<SCREEN_BLOCK_SIZE>();

    let (char_tiles, _) = mem.vram[char_base..].as_chunks::<S_TILE_SIZE>();

    let (layout_width, layout_height) = layout.layout_tile_size();

    let tile_x = usize::from(bg_4bpp.tile_x);
    let tile_y = usize::from((bg_4bpp.screen_y / 8) % u16::from(layout_height));

    let screen_block_index = (tile_y / SCREEN_BLOCK_WIDTH)
        * (usize::from(layout_width) / SCREEN_BLOCK_WIDTH)
        + (tile_x / SCREEN_BLOCK_HEIGHT);

    let inner_screen_block_index =
        (tile_y % SCREEN_BLOCK_WIDTH) * SCREEN_BLOCK_WIDTH + (tile_x % SCREEN_BLOCK_HEIGHT);

    bg_4bpp.tile_x = (bg_4bpp.tile_x + 1) % layout_width;

    let (screen_block, _) = screen_blocks[screen_block_index].as_chunks::<2>(); // screen entry is 2 bytes
    let screen_entry_bits = u16::from_le_bytes(screen_block[inner_screen_block_index]);
    let screen_entry = TextScreenEntry::from_bits(screen_entry_bits);

    let (char_entry, _) = char_tiles[screen_entry.tile_number()].as_chunks::<4>(); // each pixel row is 4 bytes

    let fine_y = usize::from(if screen_entry.vertical_flip() {
        7 - (bg_4bpp.screen_y % 8)
    } else {
        bg_4bpp.screen_y % 8
    });

    // use fine_y to select a 4 byte pixel row
    let mut char_row = if screen_entry.horizontal_flip() {
        let mut flipped = char_entry[fine_y];
        flipped.reverse();
        flipped
    } else {
        char_entry[fine_y]
    };

    // lo nibble is the left pixel while the hi nibble is the right pixel,
    // this complicates things when outputing pixels so we swap the nibbles via left rotate.
    for byte in char_row.iter_mut() {
        *byte = byte.rotate_left(4);
    }

    let palette_number = screen_entry.palette_number() as u8;
    let palette = array::from_fn(|_| (palette_number << 4) as u8 | palette_number as u8);
    bg_4bpp
        .palette_shifter
        .set_input(u32::from_be_bytes(palette));
    bg_4bpp
        .pixel_shifter
        .set_input(u32::from_be_bytes(char_row));
}

fn fetch_pixel_4bpp(bg_4bpp: &mut BackGround4bpp, mem: &Memory) -> Rgb5 {
    if bg_4bpp.pixel_x_counter % 8 == 0 {
        fetch_tile_4bpp(bg_4bpp, mem);
    }

    bg_4bpp.pixel_x_counter += 1;

    let fine_x = bg_4bpp.background.scroll_x.offset() & 7;
    let pixel_select = 4 * (7 - fine_x);

    let pixel_color = (bg_4bpp.pixel_shifter.output() >> pixel_select) & 0xF;
    let palette_number = (bg_4bpp.palette_shifter.output() >> pixel_select) & 0xF;

    bg_4bpp.pixel_shifter = Shifter4Bpp::from_bits(bg_4bpp.pixel_shifter.into_bits() << 4);
    bg_4bpp.palette_shifter = Shifter4Bpp::from_bits(bg_4bpp.palette_shifter.into_bits() << 4);

    let (palettes, _) = mem.palette_ram[BG_PALETTE].as_chunks::<PALETTE_SIZE_4BPP>();

    let (color_palette, _) = palettes[palette_number as usize].as_chunks::<2>();
    let color_bytes = u16::from_le_bytes(color_palette[pixel_color as usize]);

    Rgb5::from_u16(color_bytes)
}

pub struct BackGround4bpp {
    tile_x: u8,
    screen_y: u16,
    background: Background,
    pixel_shifter: Shifter4Bpp,
    palette_shifter: Shifter4Bpp,
    pixel_x_counter: u8,
}

impl BackGround4bpp {
    pub fn new(mem: &Memory, background: Background, scanline_y: usize) -> Self {
        let mut out = Self {
            tile_x: (background.scroll_x.offset() / 8) as u8,
            screen_y: (scanline_y + background.scroll_y.offset()) as u16,
            background,
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

pub struct Background {
    pub bg_control: BgControl,
    pub scroll_x: BgScroll,
    pub scroll_y: BgScroll,
}

#[bitfield(u64)]
struct Shifter4Bpp {
    input: u32,
    output: u32,
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
