use std::ops::Range;
use tinyvec::ArrayVec;

use crate::ppu::Ppu;
use crate::ppu::common::*;
use crate::ppu::core::PaletteRam;
use crate::ppu::fetcher::*;
use crate::ppu::registers::FrameSelect;
use crate::ppu::sprites::*;
use crate::{DISPLAY_WIDTH, Rgb5};

use BackgroundLayerType::*;

pub fn draw_mode0(ppu: &mut Ppu) {
    let enabled_backgrounds = [
        Background::new(0, ppu.registers.lcd_control.bg0_enable(), BgType::NormalBg),
        Background::new(1, ppu.registers.lcd_control.bg1_enable(), BgType::NormalBg),
        Background::new(2, ppu.registers.lcd_control.bg2_enable(), BgType::NormalBg),
        Background::new(3, ppu.registers.lcd_control.bg3_enable(), BgType::NormalBg),
    ];

    let mut background_layers = select_tiled_backgrounds(ppu, &enabled_backgrounds);
    let mut sprite_fetcher = SpriteFetcher::new(ppu);

    if background_layers.is_empty() {
        disabled_draw(ppu, sprite_fetcher);
        return;
    };

    let backdrop_color = backdrop_color(&ppu.mem.palette_ram);
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    ppu.display_buffer[scanline_y].fill_with(|| {
        let sprite_color = fetch_sprite_pixel(&mut sprite_fetcher, &ppu.mem);
        let bg_color_layers: ArrayVec<[Option<OutputPixel>; 4]> = background_layers
            .iter_mut()
            .map(|layer| layer.fetch_pixel(&ppu.mem))
            .collect();

        merge_colors(sprite_color, &bg_color_layers, backdrop_color)
    });
}

pub fn draw_mode1(ppu: &mut Ppu) {
    let enabled_backgrounds = [
        Background::new(0, ppu.registers.lcd_control.bg0_enable(), BgType::NormalBg),
        Background::new(1, ppu.registers.lcd_control.bg1_enable(), BgType::NormalBg),
        Background::new(2, ppu.registers.lcd_control.bg2_enable(), BgType::AffineBg),
    ];

    let mut background_layers = select_tiled_backgrounds(ppu, &enabled_backgrounds);
    let mut sprite_fetcher = SpriteFetcher::new(ppu);

    if background_layers.is_empty() {
        disabled_draw(ppu, sprite_fetcher);
        return;
    };

    let backdrop_color = backdrop_color(&ppu.mem.palette_ram);
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    ppu.display_buffer[scanline_y].fill_with(|| {
        let sprite_color = fetch_sprite_pixel(&mut sprite_fetcher, &ppu.mem);
        let bg_color_layers: ArrayVec<[Option<OutputPixel>; 4]> = background_layers
            .iter_mut()
            .map(|layer| layer.fetch_pixel(&ppu.mem))
            .collect();

        merge_colors(sprite_color, &bg_color_layers, backdrop_color)
    });
}

// pub fn draw_mode2(ppu: &mut Ppu) {}

pub fn draw_mode3(ppu: &mut Ppu) {
    const PIXEL_ROW_BYTE_SIZE: usize = DISPLAY_WIDTH * size_of::<u16>();
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    let mut sprite_fetcher = SpriteFetcher::new(ppu);

    if !ppu.registers.lcd_control.bg2_enable() {
        disabled_draw(ppu, sprite_fetcher);
        return;
    }

    let (vram_rows, _) = ppu.mem.vram.as_chunks::<PIXEL_ROW_BYTE_SIZE>();
    let (vram_row, remainder) = vram_rows[scanline_y].as_chunks::<2>();
    debug_assert_eq!(vram_row.len(), DISPLAY_WIDTH);
    debug_assert!(remainder.is_empty());

    let display_buffer_row = &mut ppu.display_buffer[scanline_y];
    let backdrop_color = backdrop_color(&ppu.mem.palette_ram);
    let priority = ppu.registers.bg_controls[2].bg_priority();

    for (src, dst) in vram_row
        .iter()
        .copied()
        .map(|src| {
            let sprite_color = fetch_sprite_pixel(&mut sprite_fetcher, &ppu.mem);
            let bg_color = OutputPixel {
                color: Rgb5::from(u16::from_le_bytes(src)),
                priority,
            };

            merge_colors(sprite_color, &[Some(bg_color)], backdrop_color)
        })
        .zip(display_buffer_row)
    {
        *dst = src;
    }
}

const PAGE_SIZE: usize = 40 * 1024;

/// Page 0 is the first 40k of vram
const PAGE_0: Range<usize> = 0..PAGE_SIZE;
/// Page 1 is the second 40k of vram
const PAGE_1: Range<usize> = PAGE_SIZE..(PAGE_SIZE * 2);

pub fn draw_mode4(ppu: &mut Ppu) {
    const PIXEL_ROW_BYTE_SIZE: usize = DISPLAY_WIDTH * size_of::<u8>();
    let mut sprite_fetcher = SpriteFetcher::new(ppu);

    if !ppu.registers.lcd_control.bg2_enable() {
        disabled_draw(ppu, sprite_fetcher);
        return;
    }

    let vram = match ppu.registers.lcd_control.display_frame_select() {
        FrameSelect::Page0 => &ppu.mem.vram[PAGE_0],
        FrameSelect::Page1 => &ppu.mem.vram[PAGE_1],
    };

    let backdrop_color = backdrop_color(&ppu.mem.palette_ram);
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());
    let vram_row = vram.as_chunks::<PIXEL_ROW_BYTE_SIZE>().0[scanline_y];
    let display_buffer_row = &mut ppu.display_buffer[scanline_y];
    let priority = ppu.registers.bg_controls[2].bg_priority();
    let (palettes, _) = ppu.mem.palette_ram[BG_PALETTE].as_chunks::<2>();

    for (src, dst) in vram_row
        .iter()
        .copied()
        .map(usize::from)
        .map(|palette_index| {
            let sprite_color = fetch_sprite_pixel(&mut sprite_fetcher, &ppu.mem);
            let bg_color = OutputPixel {
                color: Rgb5::from(u16::from_le_bytes(palettes[palette_index])),
                priority,
            };

            merge_colors(sprite_color, &[Some(bg_color)], backdrop_color)
        })
        .zip(display_buffer_row)
    {
        *dst = src;
    }
}

pub fn draw_mode5(ppu: &mut Ppu) {
    const MODE5_WIDTH: usize = 160;
    const PIXEL_ROW_BYTE_SIZE: usize = MODE5_WIDTH * size_of::<u16>();

    let mut sprite_fetcher = SpriteFetcher::new(ppu);
    if !ppu.registers.lcd_control.bg2_enable() {
        disabled_draw(ppu, sprite_fetcher);
        return;
    }

    let vram = match ppu.registers.lcd_control.display_frame_select() {
        FrameSelect::Page0 => &ppu.mem.vram[PAGE_0],
        FrameSelect::Page1 => &ppu.mem.vram[PAGE_1],
    };

    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    // Mode 5 is only 160 pixels in width, fill in remaining pixels on scanline with color0
    // of bg from palette ram.
    let (display_buffer_row, display_buffer_row_remaining) = {
        let display_buffer = ppu.display_buffer[scanline_y].as_chunks_mut::<MODE5_WIDTH>();
        (&mut display_buffer.0[0], display_buffer.1)
    };

    // Mode 5 is only 128 scanlines in height, remaining scanlines filled in with color0 of bg from palette ram.
    let Some(vram_row) = vram.as_chunks::<PIXEL_ROW_BYTE_SIZE>().0.get(scanline_y) else {
        disabled_draw(ppu, sprite_fetcher);
        return;
    };

    let (vram_row, _) = vram_row.as_chunks::<2>();
    let priority = ppu.registers.bg_controls[2].bg_priority();
    let backdrop_color = backdrop_color(&ppu.mem.palette_ram);

    // draw first 160 pixels
    for (src, dst) in vram_row
        .iter()
        .copied()
        .map(|src| {
            let sprite_color = fetch_sprite_pixel(&mut sprite_fetcher, &ppu.mem);
            let bg_color = OutputPixel {
                color: Rgb5::from(u16::from_le_bytes(src)),
                priority,
            };

            merge_colors(sprite_color, &[Some(bg_color)], backdrop_color)
        })
        .zip(display_buffer_row)
    {
        *dst = src;
    }

    // fill remaining (240 - 160) pixels, with backdrop color or sprite pixel
    display_buffer_row_remaining.fill_with(|| {
        match fetch_sprite_pixel(&mut sprite_fetcher, &ppu.mem) {
            Some(sp) => sp.color,
            None => backdrop_color,
        }
    });
}

#[derive(Copy, Default, Clone)]
enum BgType {
    #[default]
    NormalBg,
    AffineBg,
}

#[derive(Copy, Default, Clone)]
struct Background {
    bg_number: u8,
    enabled: bool,
    bg_type: BgType,
}

impl Background {
    pub fn new(bg_number: u8, enabled: bool, bg_type: BgType) -> Self {
        Self {
            bg_number,
            enabled,
            bg_type,
        }
    }
}

/// Returns a ArrayVec of backgrounds ordered by descending priority.
/// For backgrounds with equal priority, the priority goes as follows from
/// highest to lowest: bg0 - bg3.
/// ArrayVec will be empty if no backgrounds are enabled.
/// Panics if enabled backgrounds has len() > 4.
fn select_tiled_backgrounds(
    ppu: &Ppu,
    enabled_backgrounds: &[Background],
) -> ArrayVec<[BackgroundLayerType; 4]> {
    let scanline_y = ppu.registers.v_counter.scanline_count();
    let bg_controls_iter = enabled_backgrounds.iter().copied().filter_map(
        |Background {
             bg_number,
             enabled,
             bg_type,
         }| {
            let bg_number = usize::from(bg_number);
            let bg_control = ppu.registers.bg_controls[bg_number];

            match (enabled, bg_type) {
                (true, BgType::NormalBg) => {
                    let bg_scroll_x = ppu.registers.bg_scrolls_x[bg_number];
                    let bg_scroll_y = ppu.registers.bg_scrolls_y[bg_number];

                    Some(Normal(BackgroundLayer::new(
                        bg_control,
                        bg_scroll_x,
                        bg_scroll_y,
                        &ppu.mem,
                        scanline_y,
                    )))
                }
                (true, BgType::AffineBg) => {
                    let affine_params = match bg_number {
                        2 => &ppu.registers.bg2_affine,
                        3 => &ppu.registers.bg2_affine,
                        _ => panic!("Only background layer 2-3 can be affine!"),
                    };

                    Some(Affine(AffineBackgroundLayer::new(
                        bg_control,
                        affine_params.clone(),
                        scanline_y,
                    )))
                }
                _ => None,
            }
        },
    );

    let mut bgs: ArrayVec<[BackgroundLayerType; 4]> = bg_controls_iter.collect();
    // This must be stable sort!
    bgs.sort_by_key(|item| item.priority());
    bgs
}

fn merge_colors(
    sprite_color: Option<OutputPixel>,
    bg_color_layers: &[Option<OutputPixel>],
    backdrop_color: Rgb5,
) -> Rgb5 {
    // grab the first opaque bg color, otherwise transparent color is used
    let bg_color = bg_color_layers
        .iter()
        .find(|pixel_type| pixel_type.is_some())
        .cloned()
        .flatten();

    match (sprite_color, bg_color) {
        (None, None) => backdrop_color,
        (None, Some(bg)) => bg.color,
        (Some(sp), None) => sp.color,
        (Some(sp), Some(bg)) => {
            if sp.priority <= bg.priority {
                sp.color
            } else {
                bg.color
            }
        }
    }
}

/// Get color 0 of palette 0 in background palette which is used for
/// all transparent pixels.
fn backdrop_color(palette: &PaletteRam) -> Rgb5 {
    let color_0 = u16::from_le_bytes([palette[0], palette[1]]);
    Rgb5::from_u16(color_0)
}

/// When layer is disabled, just fill with backdrop color and render sprites if they
/// are enabled.
fn disabled_draw(ppu: &mut Ppu, mut sprite_fetcher: SpriteFetcher) {
    let backdrop_color = backdrop_color(&ppu.mem.palette_ram);
    let scanline_y = usize::from(ppu.registers.v_counter.scanline_count());

    ppu.display_buffer[scanline_y].fill_with(|| {
        match fetch_sprite_pixel(&mut sprite_fetcher, &ppu.mem) {
            Some(sp) => sp.color,
            None => backdrop_color,
        }
    });
}
