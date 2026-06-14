use crate::ppu::Ppu;
use crate::ppu::registers::FrameSelect;
use crate::{DISPLAY_WIDTH, Rgb5};
use std::ops::Range;

/// bg palette uses the first 512 bytes of palette ram
const BG_PALETTE: Range<usize> = 0..512;

/// obj palette usees the second 512 bytes of palette ram
const _OBJ_PALETTE: Range<usize> = 512..1024;

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
    let vram_row = vram_row.iter().map(|src| u16::from_le_bytes(*src));

    for (src, dst) in vram_row.zip(display_buffer_row) {
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

    let (palettes, remainder) = &ppu.mem.palette_ram[BG_PALETTE].as_chunks::<2>();
    debug_assert_eq!(palettes.len(), 256);
    debug_assert!(remainder.is_empty());

    for (palette_index, dst) in vram_row.iter().zip(display_buffer_row) {
        let color_u16 = u16::from_le_bytes(palettes[usize::from(*palette_index)]);
        *dst = Rgb5::from_u16(color_u16);
    }
}
