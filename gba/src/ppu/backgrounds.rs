use bitfield_struct::bitfield;
use std::array;
use std::ops::Range;

use crate::ppu::Ppu;
use crate::ppu::core::{Memory, PaletteRam};
use crate::ppu::registers::{BgControl, BgScroll, FrameSelect};
use crate::{DISPLAY_WIDTH, Rgb5};

/// Size in bytes for a single color palette for 4bpp tiles.
const PALETTE_SIZE_4BPP: usize = 32;
const PALETTE_REGION_SIZE: usize = 512;

/// bg palette uses the first 512 bytes of palette ram
const BG_PALETTE: Range<usize> = 0..PALETTE_REGION_SIZE;
/// obj palette usees the second 512 bytes of palette ram
const _OBJ_PALETTE: Range<usize> = 512..(PALETTE_REGION_SIZE * 2);

const CHAR_BLOCK_SIZE: usize = 16 * 1024;
const SCREEN_BLOCK_SIZE: usize = 2 * 1024;

/// Size of tiles in 4bpp format, 32 bytes big
const S_TILE_SIZE: usize = 32;
/// Size of tiles in 8bpp format, 64 bytes big
const _D_TILE_SIZE: usize = 64;

// screen block dimensions in tiles
const SCREEN_BLOCK_WIDTH: usize = 32;
const SCREEN_BLOCK_HEIGHT: usize = 32;

pub fn draw_mode0(ppu: &mut Ppu) {
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());
    let Some(background) = select_background(&ppu) else {
        ppu.display_buffer[scanline_y].fill(bg_color0(&ppu.mem.palette_ram));
        return;
    };

    let fetcher_4bpp_iter = Fetcher4BppIter::new(&ppu.mem, background, scanline_y);

    for (src, dst) in fetcher_4bpp_iter.zip(ppu.display_buffer[scanline_y].iter_mut()) {
        *dst = src;
    }
}

/// Fetcher iterator for tiles in 4bpp format.
struct Fetcher4BppIter<'a> {
    scanline_y: usize,
    tile_x: usize,
    palettes: &'a [[u8; 32]],
    screen_blocks: &'a [[u8; SCREEN_BLOCK_SIZE]],
    // slice of 32 byte tiles -> 4 bytes per 8 pixel row -> 1 byte per 2 pixels, 4 bits per pixel
    char_tiles: &'a [[u8; S_TILE_SIZE]],
    pixel_shifter: Shifter4Bpp,
    palette_shifter: Shifter4Bpp,
    pixel_x_counter: u32,
    background: BackGround,
}

impl<'a> Fetcher4BppIter<'a> {
    fn new(mem: &'a Memory, background: BackGround, scanline_y: usize) -> Self {
        let BackGround {
            bg_control,
            scroll_x,
            ..
        } = background;

        let char_base = usize::from(bg_control.char_base_block()) * CHAR_BLOCK_SIZE;
        let screen_base = usize::from(bg_control.screen_base_block()) * SCREEN_BLOCK_SIZE;
        let layout = bg_control.screen_size();

        let (palettes, remainder) = mem.palette_ram[BG_PALETTE].as_chunks::<PALETTE_SIZE_4BPP>();

        debug_assert_eq!(palettes.len(), 16);
        debug_assert!(remainder.is_empty());

        let (screen_blocks, remainder) = mem.vram
            [screen_base..screen_base + (SCREEN_BLOCK_SIZE * layout.get_block_count())]
            .as_chunks::<SCREEN_BLOCK_SIZE>();

        debug_assert!(matches!(screen_blocks.len(), 1 | 2 | 4)); // block count must be 1, 2, or 4.
        debug_assert!(remainder.is_empty());

        let (char_tiles, _) = mem.vram[char_base..].as_chunks::<S_TILE_SIZE>();

        let mut out = Self {
            scanline_y,
            tile_x: scroll_x.offset() / 8,
            palettes,
            screen_blocks,
            char_tiles,
            background,
            pixel_shifter: Shifter4Bpp::default(),
            palette_shifter: Shifter4Bpp::default(),
            pixel_x_counter: 0,
        };

        // call next 8 times to fill in output shift register with 8 pixels
        for _ in 0..8 {
            out.next();
        }

        out
    }

    fn fetch_tile(&mut self) {
        let (layout_width, layout_height) =
            self.background.bg_control.screen_size().layout_tile_size();

        let screen_y = self.scanline_y + self.background.scroll_y.offset();
        let tile_y = (screen_y / 8) % layout_height;

        let screen_block_index = (tile_y / SCREEN_BLOCK_WIDTH)
            * (layout_width / SCREEN_BLOCK_WIDTH)
            + (self.tile_x / SCREEN_BLOCK_HEIGHT);

        let inner_screen_block_index = (tile_y % SCREEN_BLOCK_WIDTH) * SCREEN_BLOCK_WIDTH
            + (self.tile_x % SCREEN_BLOCK_HEIGHT);

        self.tile_x = (self.tile_x + 1) % layout_width;

        let (screen_block, _) = self.screen_blocks[screen_block_index].as_chunks::<2>();
        let screen_entry_bytes = u16::from_le_bytes(screen_block[inner_screen_block_index]);
        let screen_entry = TextScreenEntry::from_bits(screen_entry_bytes);

        let (char_entry, _) = self.char_tiles[screen_entry.tile_number()].as_chunks::<4>();

        let fine_y = if screen_entry.vertical_flip() {
            7 - (screen_y % 8)
        } else {
            screen_y % 8
        };

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
        self.palette_shifter.set_input(u32::from_be_bytes(palette));
        self.pixel_shifter.set_input(u32::from_be_bytes(char_row));
    }
}

impl<'a> Iterator for Fetcher4BppIter<'a> {
    type Item = Rgb5;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pixel_x_counter % 8 == 0 {
            self.fetch_tile();
        }

        self.pixel_x_counter += 1;

        let fine_x = self.background.scroll_x.offset() & 7;
        let pixel_select = 4 * (7 - fine_x);

        let pixel_color = (self.pixel_shifter.output() >> pixel_select) & 0xF;
        let palette_number = (self.palette_shifter.output() >> pixel_select) & 0xF;

        self.pixel_shifter = Shifter4Bpp::from_bits(self.pixel_shifter.into_bits() << 4);
        self.palette_shifter = Shifter4Bpp::from_bits(self.palette_shifter.into_bits() << 4);

        let (color_palette, _) = self.palettes[palette_number as usize].as_chunks::<2>();
        let color_bytes = u16::from_le_bytes(color_palette[pixel_color as usize]);
        Some(Rgb5::from_u16(color_bytes))
    }
}

#[bitfield(u64)]
struct Shifter4Bpp {
    input: u32,
    output: u32,
}

pub fn draw_mode3(ppu: &mut Ppu) {
    const PIXEL_ROW_BYTE_SIZE: usize = DISPLAY_WIDTH * size_of::<u16>();
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    if !ppu.registers.lcd_control.bg2_enable() {
        ppu.display_buffer[scanline_y].fill(bg_color0(&ppu.mem.palette_ram));
        return;
    }

    let vram_row = ppu
        .mem
        .vram
        .chunks(PIXEL_ROW_BYTE_SIZE)
        .nth(scanline_y)
        .unwrap();

    let (vram_row, remainder) = vram_row.as_chunks::<2>();
    debug_assert_eq!(vram_row.len(), DISPLAY_WIDTH);
    debug_assert!(remainder.is_empty());

    let display_buffer_row = &mut ppu.display_buffer[scanline_y];

    for (src, dst) in vram_row
        .iter()
        .map(|src| u16::from_le_bytes(*src))
        .zip(display_buffer_row)
    {
        *dst = Rgb5::from_u16(src);
    }
}

const PAGE_SIZE: usize = 40 * 1024;

/// Page 0 is the first 40k of vram
const PAGE_0: Range<usize> = 0..PAGE_SIZE;
/// Page 1 is the second 40k of vram
const PAGE_1: Range<usize> = PAGE_SIZE..(PAGE_SIZE * 2);

pub fn draw_mode4(ppu: &mut Ppu) {
    const PIXEL_ROW_BYTE_SIZE: usize = DISPLAY_WIDTH * size_of::<u8>();
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    if !ppu.registers.lcd_control.bg2_enable() {
        ppu.display_buffer[scanline_y].fill(bg_color0(&ppu.mem.palette_ram));
        return;
    }

    let vram = match ppu.registers.lcd_control.display_frame_select() {
        FrameSelect::Page0 => &ppu.mem.vram[PAGE_0],
        FrameSelect::Page1 => &ppu.mem.vram[PAGE_1],
    };

    let vram_row = vram.chunks(PIXEL_ROW_BYTE_SIZE).nth(scanline_y).unwrap();
    debug_assert_eq!(vram_row.len(), PIXEL_ROW_BYTE_SIZE);

    let display_buffer_row = &mut ppu.display_buffer[scanline_y];
    debug_assert_eq!(display_buffer_row.len(), vram_row.len());

    let (palettes, remainder) = ppu.mem.palette_ram[BG_PALETTE].as_chunks::<2>();
    debug_assert_eq!(palettes.len(), 256);
    debug_assert!(remainder.is_empty());

    for (src, dst) in vram_row
        .iter()
        .map(|palette_index| u16::from_le_bytes(palettes[usize::from(*palette_index)]))
        .zip(display_buffer_row)
    {
        *dst = Rgb5::from_u16(src);
    }
}

pub fn draw_mode5(ppu: &mut Ppu) {
    const MODE5_WIDTH: usize = 160;
    const PIXEL_ROW_BYTE_SIZE: usize = MODE5_WIDTH * size_of::<u16>();

    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    if !ppu.registers.lcd_control.bg2_enable() {
        ppu.display_buffer[scanline_y].fill(bg_color0(&ppu.mem.palette_ram));
        return;
    }

    let vram = match ppu.registers.lcd_control.display_frame_select() {
        FrameSelect::Page0 => &ppu.mem.vram[PAGE_0],
        FrameSelect::Page1 => &ppu.mem.vram[PAGE_1],
    };

    let display_buffer_row = &mut ppu.display_buffer[scanline_y];

    // Mode 5 is only 128 scanlines in height, remaining scanlines filled in with color0 of bg from palette ram.
    let Some(vram_row) = vram.chunks(PIXEL_ROW_BYTE_SIZE).nth(scanline_y) else {
        display_buffer_row.fill(bg_color0(&ppu.mem.palette_ram));
        return;
    };

    let (vram_row, remainder) = vram_row.as_chunks::<2>();
    debug_assert_eq!(vram_row.len(), MODE5_WIDTH);
    debug_assert!(remainder.is_empty());

    for (src, dst) in vram_row
        .iter()
        .map(|src| u16::from_le_bytes(*src))
        .zip(display_buffer_row)
    {
        *dst = Rgb5::from_u16(src);
    }

    // Mode 5 is only 160 pixels in width, fill in remaining pixels on scanline with color0
    // of bg from palette ram.
    let display_buffer_row = &mut ppu.display_buffer[scanline_y];
    let remaining = &mut display_buffer_row[MODE5_WIDTH..];
    remaining.fill(bg_color0(&ppu.mem.palette_ram));
}

fn bg_color0(palette: &PaletteRam) -> Rgb5 {
    let color_0 = palette.first_chunk::<2>().unwrap();
    let color_u16 = u16::from_le_bytes(*color_0);
    Rgb5::from_u16(color_u16)
}

struct BackGround {
    bg_control: BgControl,
    scroll_x: BgScroll,
    scroll_y: BgScroll,
}

/// Retrieve the enabled background with the highest priority.
/// Returns None if no backgrounds are enabled.
fn select_background(ppu: &Ppu) -> Option<BackGround> {
    #[rustfmt::skip]
    let bg_controls = [
        (ppu.registers.lcd_control.bg0_enable(), ppu.registers.bg0_control, ppu.registers.bg0_scroll_x, ppu.registers.bg0_scroll_y),
        (ppu.registers.lcd_control.bg1_enable(), ppu.registers.bg1_control, ppu.registers.bg1_scroll_x, ppu.registers.bg1_scroll_y),
        (ppu.registers.lcd_control.bg2_enable(), ppu.registers.bg2_control, ppu.registers.bg2_scroll_x, ppu.registers.bg2_scroll_y),
        (ppu.registers.lcd_control.bg3_enable(), ppu.registers.bg3_control, ppu.registers.bg3_scroll_x, ppu.registers.bg3_scroll_y),
    ];

    bg_controls
        .into_iter()
        .filter_map(|(enabled, bg_control, scroll_x, scroll_y)| {
            enabled.then(|| BackGround {
                bg_control,
                scroll_x,
                scroll_y,
            })
        })
        .min_by_key(|BackGround { bg_control, .. }| bg_control.bg_priority())
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
