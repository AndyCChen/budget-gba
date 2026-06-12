use crate::ppu::Ppu;
use crate::{DISPLAY_WIDTH, Rgb5};

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
    debug_assert!(vram_row.len() == DISPLAY_WIDTH);
    debug_assert!(remainder.is_empty());

    let display_buffer_row = &mut ppu.display_buffer[scanline_y];

    let vram_row = vram_row.iter().map(|src| u16::from_le_bytes(*src));
    vram_row
        .zip(display_buffer_row.iter_mut())
        .for_each(|(src, dst)| *dst = Rgb5::from_u16(src));
}
