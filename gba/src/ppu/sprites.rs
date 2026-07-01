use bitfield_struct::bitenum;
use register_macros::gba_register;
use tinyvec::ArrayVec;

use crate::Rgb5;
use crate::ppu::Ppu;
use crate::ppu::common::PaletteType;

pub fn fetch_sprite_pixel(sprite_fetcher: &mut SpriteFetcher) -> Rgb5 {
    todo!()
}

pub struct SpriteFetcher {
    /// hold buffer of sprites to be rendered on current scanline
    sprite_buffer: ArrayVec<[u8; 128]>,
}

impl SpriteFetcher {
    pub fn new(ppu: &Ppu) -> Self {
        let mut out = Self {
            sprite_buffer: ArrayVec::new(),
        };
        sprite_scan(&mut out, ppu);
        out
    }
}

struct OamEntry {
    attribute0: Attribute0,
    attribute1: Attribute1,
    attribute2: Attribute2,
    padding: u16,
}

impl OamEntry {
    fn new(entry: &[u8; 8]) -> Self {
        let attribute0 = Attribute0::from_bits(u16::from_le_bytes([entry[0], entry[1]]));
        let attribute1 = Attribute1::from_bits(u16::from_le_bytes([entry[2], entry[3]]));
        let attribute2 = Attribute2::from_bits(u16::from_le_bytes([entry[4], entry[5]]));
        let padding = u16::from_le_bytes([entry[6], entry[7]]);

        Self {
            attribute0,
            attribute1,
            attribute2,
            padding,
        }
    }
}

const OAM_ENTRY_SIZE: usize = 8;

/// Selects all sprites that are present on current scanline for rendering.
fn sprite_scan(sprite_fetcher: &mut SpriteFetcher, ppu: &Ppu) {
    let (oam, _) = ppu.mem.oam.as_chunks::<OAM_ENTRY_SIZE>();
    let scanline_y = ppu.registers.v_counter.scanline_count();

    for (oam_number, oam_entry) in oam.iter().map(|entry| OamEntry::new(entry)).enumerate() {
        let (_, height) = get_sprite_size(&oam_entry);
        let y_start = oam_entry.attribute0.y_coord();

        let sprite_present = if let Some(y_end) = y_start.checked_add(height) {
            scanline_y >= y_start && scanline_y <= y_end
        } else {
            scanline_y <= y_start.wrapping_add(height)
        };

        if sprite_present {
            sprite_fetcher.sprite_buffer.push(oam_number as u8);
        }
    }
}

#[gba_register(u16)]
struct Attribute0 {
    y_coord: u8,

    #[bits(2, default = ObjectMode::Normal)]
    object_mode: ObjectMode,

    #[bits(2, default = GfxMode::Normal)]
    gfx_mode: GfxMode,

    enable_mosaic: bool,

    #[bits(1, default = PaletteType::ColorDepth4Bit )]
    palette_type: PaletteType,

    #[bits(2, default = Shape::Square)]
    shape: Shape,
}

impl Attribute0 {
    #[rustfmt::skip]
    fn _is_affine(&self) -> bool {
        matches!(self.object_mode(), ObjectMode::Affine | ObjectMode::AffineDouble)
    }
}

#[gba_register(u16)]
struct Attribute1 {
    #[bits(9)]
    x_coord: u16,

    #[bits(5)]
    affine_index_or_flip: u8,

    #[bits(2)]
    sprite_size: usize,
}

impl Attribute1 {
    fn _affine_index(&self) -> u8 {
        self.affine_index_or_flip()
    }

    fn horizontal_flip(&self) -> bool {
        (self.affine_index_or_flip() >> 3) & 1 == 1
    }

    fn vertical_flip(&self) -> bool {
        (self.affine_index_or_flip() >> 4) & 1 == 1
    }
}

#[gba_register(u16)]
struct Attribute2 {
    #[bits(10)]
    tile_index: u16,

    #[bits(2)]
    priority: u8,

    #[bits(4)]
    palette_number: usize,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
enum ObjectMode {
    #[fallback]
    Normal = 0,
    Affine,
    Disabled,
    AffineDouble,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
enum GfxMode {
    #[fallback]
    Normal = 0,
    AlphaBlend,
    Window,
    Forbidden,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
enum Shape {
    #[fallback]
    Square = 0,
    Wide,
    Tall,
}

static SPRITE_SIZE_TABLE: [[(u8, u8); 4]; 3] = [
    [(8, 8), (16, 16), (32, 32), (64, 64)],
    [(16, 8), (32, 8), (32, 16), (64, 32)],
    [(8, 16), (8, 32), (16, 32), (32, 64)],
];

/// Retrive dimensions of the sprite as a tuple (width, height)
fn get_sprite_size(oam_entry: &OamEntry) -> (u8, u8) {
    let size = oam_entry.attribute1.sprite_size();

    match oam_entry.attribute0.shape() {
        Shape::Square => SPRITE_SIZE_TABLE[0][size],
        Shape::Wide => SPRITE_SIZE_TABLE[1][size],
        Shape::Tall => SPRITE_SIZE_TABLE[2][size],
    }
}
