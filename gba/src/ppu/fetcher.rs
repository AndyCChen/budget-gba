use bitfield_struct::bitfield;
use std::array;

use crate::Rgb5;
use crate::ppu::common::*;
use crate::ppu::core::Memory;
use crate::ppu::registers::{BgControl, BgScroll};

#[derive(Default)]
pub struct BackgroundLayer {
    bg_control: BgControl,
    scroll_x: BgScroll,
    tile_x: u8,
    y_coord: u16,
    pixel_counter_x: u8,
    shifter_type: ShifterType,
}

impl BackgroundLayer {
    pub fn new(
        bg_control: BgControl,
        scroll_x: BgScroll,
        scroll_y: BgScroll,
        mem: &Memory,
        scanline_y: usize,
    ) -> Self {
        let mut layer = Self {
            bg_control,
            scroll_x,
            tile_x: (scroll_x.offset() / 8) as u8,
            y_coord: (scanline_y + scroll_y.offset()) as u16,
            pixel_counter_x: 0,
            shifter_type: match bg_control.palette_type() {
                PaletteType::ColorDepth4Bit => ShifterType4bpp {
                    pixel_shifter: Shifter4Bpp::default(),
                    palette_shifter: Shifter4Bpp::default(),
                },
                PaletteType::ColorDepth8Bit => ShifterType8bpp(Shifter8Bpp::default()),
            },
        };

        for _ in 0..8 {
            fetch_normal_pixel(&mut layer, mem);
        }

        layer
    }

    pub fn priority(&self) -> u8 {
        self.bg_control.bg_priority()
    }
}

/// Fetch pixel from normal mode background layers (non-affine).
/// If pixel is transparent, return None,
/// else the opaque color is wrapped in Some.
pub fn fetch_normal_pixel(layer: &mut BackgroundLayer, mem: &Memory) -> Option<OutputPixel> {
    if layer.pixel_counter_x.is_multiple_of(8) {
        fetch_normal_tile(layer, mem);
    }

    layer.pixel_counter_x += 1;
    let fine_x = layer.scroll_x.offset() & 7;

    match &mut layer.shifter_type {
        ShifterType4bpp {
            pixel_shifter,
            palette_shifter,
        } => {
            let pixel_select = S_TILE_ROW_SIZE * (7 - fine_x);

            // index within a 16 color palette
            let color_index = ((pixel_shifter.output() >> pixel_select) & 0xF) as usize;
            // index to select one of the 16 palettes
            let palette_selection = ((palette_shifter.output() >> pixel_select) & 0xF) as usize;

            *pixel_shifter = Shifter4Bpp::from_bits(pixel_shifter.into_bits() << 4);
            *palette_shifter = Shifter4Bpp::from_bits(palette_shifter.into_bits() << 4);

            if color_index != 0 {
                let (palettes, _) = mem.palette_ram[BG_PALETTE].as_chunks::<PALETTE_SIZE_4BPP>();
                let (color_palette, _) = palettes[palette_selection].as_chunks::<RGB5_SIZE>();
                let color_bits = u16::from_le_bytes(color_palette[color_index]);

                Some(OutputPixel {
                    color: Rgb5::from_u16(color_bits),
                    priority: layer.bg_control.bg_priority(),
                })
            } else {
                None
            }
        }
        ShifterType8bpp(pixel_shifter) => {
            let pixel_select = D_TILE_ROW_SIZE * (7 - fine_x);

            // index within a 256 color palette
            let color_index = ((pixel_shifter.output() >> pixel_select) & 0xFF) as usize;
            *pixel_shifter = Shifter8Bpp::from_bits(pixel_shifter.into_bits() << 8);

            if color_index != 0 {
                // palette for 8bpp mode is one big palette with 256 colors
                let (palette, _) = mem.palette_ram[BG_PALETTE].as_chunks::<RGB5_SIZE>();
                let color_bits = u16::from_le_bytes(palette[color_index]);

                Some(OutputPixel {
                    color: Rgb5::from_u16(color_bits),
                    priority: layer.bg_control.bg_priority(),
                })
            } else {
                None
            }
        }
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

enum ShifterType {
    ShifterType4bpp {
        pixel_shifter: Shifter4Bpp,
        palette_shifter: Shifter4Bpp,
    },
    ShifterType8bpp(Shifter8Bpp),
}

impl Default for ShifterType {
    fn default() -> Self {
        Self::ShifterType4bpp {
            pixel_shifter: Shifter4Bpp::default(),
            palette_shifter: Shifter4Bpp::default(),
        }
    }
}

use ShifterType::*;

/// Fetch tiles for normal mode backgrounds (non-affine)
fn fetch_normal_tile(layer: &mut BackgroundLayer, mem: &Memory) {
    let char_base = usize::from(layer.bg_control.char_base_block()) * CHAR_BLOCK_SIZE;
    let screen_base = usize::from(layer.bg_control.screen_base_block()) * SCREEN_BLOCK_SIZE;
    let layout = layer.bg_control.screen_size();

    let (layout_width_tiles, layout_height_tiles) = layout.layout_tile_size();

    let tile_x = usize::from(layer.tile_x % layout_width_tiles);
    let tile_y = usize::from((layer.y_coord / 8) % u16::from(layout_height_tiles));

    let screen_block_index = (tile_y / SCREEN_BLOCK_WIDTH)
        * (usize::from(layout_width_tiles) / SCREEN_BLOCK_WIDTH)
        + (tile_x / SCREEN_BLOCK_HEIGHT);

    let inner_screen_block_index =
        (tile_y % SCREEN_BLOCK_WIDTH) * SCREEN_BLOCK_WIDTH + (tile_x % SCREEN_BLOCK_HEIGHT);

    layer.tile_x += 1;

    let (screen_blocks, _) = mem.vram
        [screen_base..screen_base + (SCREEN_BLOCK_SIZE * layout.get_block_count())]
        .as_chunks::<SCREEN_BLOCK_SIZE>();

    let (screen_block, _) = screen_blocks[screen_block_index].as_chunks::<SCREEN_ENTRY_SIZE>();
    let screen_entry_bits = u16::from_le_bytes(screen_block[inner_screen_block_index]);
    let screen_entry = TextScreenEntry::from_bits(screen_entry_bits);

    let fine_y = usize::from(if screen_entry.vertical_flip() {
        7 - (layer.y_coord % 8)
    } else {
        layer.y_coord % 8
    });

    match &mut layer.shifter_type {
        ShifterType4bpp {
            pixel_shifter,
            palette_shifter,
        } => {
            let (tiles, _) = mem.vram[char_base..].as_chunks::<S_TILE_SIZE>();
            let (tile_rows, _) = tiles[screen_entry.tile_number()].as_chunks::<S_TILE_ROW_SIZE>();

            // use fine_y to select a 4 byte pixel row
            let mut pixel_row = if screen_entry.horizontal_flip() {
                let mut flipped = tile_rows[fine_y];
                flipped.reverse();
                flipped
            } else {
                tile_rows[fine_y]
            };

            // lo nibble is the left pixel while the hi nibble is the right pixel,
            // this complicates things when outputing pixels so we swap the nibbles via left rotate.
            pixel_row
                .iter_mut()
                .for_each(|byte| *byte = byte.rotate_right(4));

            let palette_number = screen_entry.palette_number() as u8;
            let palette = array::from_fn(|_| (palette_number << 4) | palette_number);

            palette_shifter.set_input(u32::from_be_bytes(palette));
            pixel_shifter.set_input(u32::from_be_bytes(pixel_row));
        }
        ShifterType8bpp(pixel_shifter) => {
            let (tiles, _) = mem.vram[char_base..].as_chunks::<D_TILE_SIZE>();
            let (tile_rows, _) = tiles[screen_entry.tile_number()].as_chunks::<D_TILE_ROW_SIZE>();

            // use fine_y to select a 8 byte pixel row
            let pixel_row = if screen_entry.horizontal_flip() {
                let mut flipped = tile_rows[fine_y];
                flipped.reverse();
                flipped
            } else {
                tile_rows[fine_y]
            };

            pixel_shifter.set_input(u64::from_be_bytes(pixel_row));
        }
    };
}
