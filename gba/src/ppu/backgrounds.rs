use bitfield_struct::bitfield;

use crate::ppu::Ppu;
use crate::ppu::core::PaletteRam;
use crate::ppu::registers::{BgControl, BgScroll, FrameSelect};
use crate::{DISPLAY_WIDTH, Rgb5};
use std::ops::Range;

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

    let Some(BackGround {
        bg_control,
        scroll_x: _scroll_x,
        scroll_y: _scroll_y,
    }) = select_background(&ppu)
    else {
        ppu.display_buffer[scanline_y].fill(bg_color0(&ppu.mem.palette_ram));
        return;
    };

    let char_base = usize::from(bg_control.char_base_block()) * CHAR_BLOCK_SIZE;
    let screen_base = usize::from(bg_control.screen_base_block()) * SCREEN_BLOCK_SIZE;
    let layout = bg_control.screen_size();
    let _palette_type = bg_control.palettes();

    let (palettes, remainder) = ppu.mem.palette_ram[BG_PALETTE].as_chunks::<32>();
    debug_assert_eq!(palettes.len(), 16);
    debug_assert!(remainder.is_empty());

    // TODO: handle 64 byte format
    let (char_block, _) = ppu.mem.vram[char_base..].as_chunks::<S_TILE_SIZE>();

    let (screen_blocks, remainder) = ppu.mem.vram
        [screen_base..screen_base + (SCREEN_BLOCK_SIZE * layout.get_block_count())]
        .as_chunks::<SCREEN_BLOCK_SIZE>();

    debug_assert!(matches!(screen_blocks.len(), 1 | 2 | 4)); // block count must be 1, 2, or 4.
    debug_assert!(remainder.is_empty());

    let mut display_buffer_row = ppu.display_buffer[scanline_y].iter_mut();
    let (layout_width, _) = layout.layout_tile_size();
    let tile_y = scanline_y / 8;

    for tile_x in 0..30 {
        let screen_block_index = (tile_y / SCREEN_BLOCK_WIDTH)
            * (layout_width / SCREEN_BLOCK_WIDTH)
            + (tile_x / SCREEN_BLOCK_HEIGHT);

        let inner_screen_block_index =
            (tile_y % SCREEN_BLOCK_WIDTH) * SCREEN_BLOCK_WIDTH + (tile_x % SCREEN_BLOCK_HEIGHT);

        let lo = screen_blocks[screen_block_index][inner_screen_block_index * 2];
        let hi = screen_blocks[screen_block_index][inner_screen_block_index * 2 + 1];
        let screen_entry = TextScreenEntry::from_bits(u16::from_le_bytes([lo, hi]));
        let (char_entry, _) = char_block[screen_entry.tile_number()].as_chunks::<4>();
        let fine_y = scanline_y % 8;

        for (left_pixel, right_pixel) in char_entry[fine_y]
            .iter()
            .map(|byte| usize::from(*byte))
            .map(|byte| (byte & 0xF, (byte >> 4) & 0xF))
        {
            let (color_palette, _) = palettes[screen_entry.palette_number()].as_chunks::<2>();
            let left_color = Rgb5::from_u16(u16::from_le_bytes(color_palette[left_pixel]));
            let right_color = Rgb5::from_u16(u16::from_le_bytes(color_palette[right_pixel]));
            *display_buffer_row.next().unwrap() = left_color;
            *display_buffer_row.next().unwrap() = right_color;
        }
    }
}

pub fn draw_mode3(ppu: &mut Ppu) {
    if !ppu.registers.lcd_control.bg2_enable() {
        return;
    }

    const PIXEL_ROW_BYTE_SIZE: usize = DISPLAY_WIDTH * size_of::<u16>();
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

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
    if !ppu.registers.lcd_control.bg2_enable() {
        return;
    }

    const PIXEL_ROW_BYTE_SIZE: usize = DISPLAY_WIDTH * size_of::<u8>();
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

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
    if !ppu.registers.lcd_control.bg2_enable() {
        return;
    }

    const MODE5_WIDTH: usize = 160;
    const PIXEL_ROW_BYTE_SIZE: usize = MODE5_WIDTH * size_of::<u16>();

    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

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
    verical_flip: bool,

    #[bits(4)]
    palette_number: usize,
}
