use std::ops::Range;
use tinyvec::ArrayVec;

use crate::ppu::Ppu;
use crate::ppu::common::*;
use crate::ppu::core::PaletteRam;
use crate::ppu::fetcher::*;
use crate::ppu::registers::FrameSelect;
use crate::{DISPLAY_WIDTH, Rgb5};

pub fn draw_mode0(ppu: &mut Ppu) {
    let backdrop_color = backdrop_color(&ppu.mem.palette_ram);
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    let mut background_fetchers = select_backgrounds(ppu);
    if background_fetchers.is_empty() {
        ppu.display_buffer[scanline_y].fill(backdrop_color);
        return;
    };

    for dst in ppu.display_buffer[scanline_y].iter_mut() {
        let bg_color_layers: ArrayVec<[PixelType; 4]> = background_fetchers
            .iter_mut()
            .map(|bg| fetch_pixel(&ppu.mem, bg))
            .collect();

        let color = merge_colors(&bg_color_layers, backdrop_color);
        *dst = color;
    }
}

pub fn draw_mode3(ppu: &mut Ppu) {
    const PIXEL_ROW_BYTE_SIZE: usize = DISPLAY_WIDTH * size_of::<u16>();
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    if !ppu.registers.lcd_control.bg2_enable() {
        ppu.display_buffer[scanline_y].fill(backdrop_color(&ppu.mem.palette_ram));
        return;
    }

    let (vram_rows, _) = ppu.mem.vram.as_chunks::<PIXEL_ROW_BYTE_SIZE>();
    let (vram_row, remainder) = vram_rows[scanline_y].as_chunks::<2>();
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
        ppu.display_buffer[scanline_y].fill(backdrop_color(&ppu.mem.palette_ram));
        return;
    }

    let vram = match ppu.registers.lcd_control.display_frame_select() {
        FrameSelect::Page0 => &ppu.mem.vram[PAGE_0],
        FrameSelect::Page1 => &ppu.mem.vram[PAGE_1],
    };

    let vram_row = vram.as_chunks::<PIXEL_ROW_BYTE_SIZE>().0[scanline_y];
    let display_buffer_row = &mut ppu.display_buffer[scanline_y];

    let (palettes, _) = ppu.mem.palette_ram[BG_PALETTE].as_chunks::<2>();

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
        ppu.display_buffer[scanline_y].fill(backdrop_color(&ppu.mem.palette_ram));
        return;
    }

    let vram = match ppu.registers.lcd_control.display_frame_select() {
        FrameSelect::Page0 => &ppu.mem.vram[PAGE_0],
        FrameSelect::Page1 => &ppu.mem.vram[PAGE_1],
    };

    let display_buffer_row = &mut ppu.display_buffer[scanline_y];

    // Mode 5 is only 128 scanlines in height, remaining scanlines filled in with color0 of bg from palette ram.
    let Some(vram_row) = vram.as_chunks::<PIXEL_ROW_BYTE_SIZE>().0.get(scanline_y) else {
        display_buffer_row.fill(backdrop_color(&ppu.mem.palette_ram));
        return;
    };

    let (vram_row, _) = vram_row.as_chunks::<2>();

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
    remaining.fill(backdrop_color(&ppu.mem.palette_ram));
}

/// Returns a ArrayVec of backgrounds ordered by descending priority.
/// For backgrounds with equal priority, the priority goes as follows from
/// highest to lowest: bg0 - bg3.
/// ArrayVec will be empty if no backgrounds are enabled.
fn select_backgrounds(ppu: &Ppu) -> ArrayVec<[FetchType; 4]> {
    #[rustfmt::skip]
    let bg_controls = [
        (ppu.registers.lcd_control.bg0_enable(), ppu.registers.bg0_control, ppu.registers.bg0_scroll_x, ppu.registers.bg0_scroll_y),
        (ppu.registers.lcd_control.bg1_enable(), ppu.registers.bg1_control, ppu.registers.bg1_scroll_x, ppu.registers.bg1_scroll_y),
        (ppu.registers.lcd_control.bg2_enable(), ppu.registers.bg2_control, ppu.registers.bg2_scroll_x, ppu.registers.bg2_scroll_y),
        (ppu.registers.lcd_control.bg3_enable(), ppu.registers.bg3_control, ppu.registers.bg3_scroll_x, ppu.registers.bg3_scroll_y),
    ];

    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());
    let bg_controls_iter = bg_controls
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
        });

    let mut bgs: ArrayVec<[FetchType; 4]> = bg_controls_iter.collect();
    // This must be stable sort!
    bgs.sort();
    bgs
}

fn merge_colors(bg_color_layers: &[PixelType], backdrop_color: Rgb5) -> Rgb5 {
    if let Some(opaque_color) =
        bg_color_layers
            .iter()
            .copied()
            .find_map(|pixel_type| match pixel_type {
                PixelType::Opaque { color, .. } => Some(color),
                PixelType::Transparent => None,
            })
    {
        opaque_color
    } else {
        backdrop_color
    }
}

fn backdrop_color(palette: &PaletteRam) -> Rgb5 {
    let color_0 = u16::from_le_bytes([palette[0], palette[1]]);
    Rgb5::from_u16(color_0)
}
