use std::ops::Range;

use crate::ppu::Ppu;
use crate::ppu::common::*;
use crate::ppu::core::PaletteRam;
use crate::ppu::fetcher::*;
use crate::ppu::registers::{FrameSelect, PaletteType};
use crate::{DISPLAY_WIDTH, Rgb5};

pub fn draw_mode0(ppu: &mut Ppu) {
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    let Some(mut backgrounds) = select_backgrounds(ppu) else {
        ppu.display_buffer[scanline_y].fill(bg_color0(&ppu.mem.palette_ram));
        return;
    };

    let mut colors: Vec<PixelType> = Vec::with_capacity(4);

    for dst in ppu.display_buffer[scanline_y].iter_mut() {
        colors.extend(backgrounds.iter_mut().map(|bg| fetch_pixel(&ppu.mem, bg)));
        *dst = merge_bg_colors(&colors);
        colors.clear();
    }
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

/// Returns a collects of backgrounds ordered by descending priority.
/// Returns None if no backgrounds are enabled.
fn select_backgrounds(ppu: &Ppu) -> Option<Vec<FetchType>> {
    #[rustfmt::skip]
    let bg_controls = [
        (ppu.registers.lcd_control.bg0_enable(), ppu.registers.bg0_control, ppu.registers.bg0_scroll_x, ppu.registers.bg0_scroll_y),
        (ppu.registers.lcd_control.bg1_enable(), ppu.registers.bg1_control, ppu.registers.bg1_scroll_x, ppu.registers.bg1_scroll_y),
        (ppu.registers.lcd_control.bg2_enable(), ppu.registers.bg2_control, ppu.registers.bg2_scroll_x, ppu.registers.bg2_scroll_y),
        (ppu.registers.lcd_control.bg3_enable(), ppu.registers.bg3_control, ppu.registers.bg3_scroll_x, ppu.registers.bg3_scroll_y),
    ];

    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    let mut bgs: Vec<FetchType> = bg_controls
        .into_iter()
        .filter_map(|(enabled, bg_control, scroll_x, scroll_y)| {
            enabled.then(|| Background {
                bg_control,
                scroll_x,
                scroll_y,
            })
        })
        .map(|background| match background.bg_control.palettes() {
            PaletteType::ColorDepth4Bit => {
                FetchType::Fetch4bpp(BackGround4bpp::new(&ppu.mem, background, scanline_y))
            }
            PaletteType::ColorDepth8Bit => {
                FetchType::Fetch8bpp(BackGround8bpp::new(&ppu.mem, background, scanline_y))
            }
        })
        .collect();

    bgs.sort();

    if bgs.is_empty() { None } else { Some(bgs) }
}

fn merge_bg_colors(pixel_types: &[PixelType]) -> Rgb5 {
    let mut final_color = Rgb5::default();
    let mut is_opaque = false;

    for pixel_type in pixel_types.iter().rev() {
        match pixel_type {
            PixelType::Opaque(rgb5) => {
                final_color = *rgb5;
                is_opaque = true;
            }
            PixelType::Transparent(rgb5) => {
                if !is_opaque {
                    final_color = *rgb5;
                }
            }
        }
    }

    final_color
}
