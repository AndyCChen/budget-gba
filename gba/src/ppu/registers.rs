use bitfield_struct::bitenum;
use register_macros::gba_register;

use crate::ppu::common::{PaletteType, Vector2};

pub struct Registers {
    pub lcd_control: LcdControl,
    pub lcd_status: LcdStatus,
    pub v_counter: VerticalCounter,
    pub bg_controls: [BgControl; 4],
    pub bg_scrolls_x: [BgScroll; 4],
    pub bg_scrolls_y: [BgScroll; 4],
    pub bg2_affine: AffineParameters,
    pub bg3_affine: AffineParameters,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            lcd_control: LcdControl::default(),
            lcd_status: LcdStatus::default(),
            v_counter: VerticalCounter::default(),
            bg_controls: [BgControl::default(); 4],
            bg_scrolls_x: [BgScroll::default(); 4],
            bg_scrolls_y: [BgScroll::default(); 4],
            bg2_affine: AffineParameters::default(),
            bg3_affine: AffineParameters::default(),
        }
    }
}

#[gba_register(u16)]
pub struct LcdControl {
    #[bits(3, default = BgMode::Mode0, from = BgMode::from_bits)]
    pub bg_mode: BgMode,
    pub cgb_mode: bool, // only set by bios

    #[bits(1, default = FrameSelect::Page0)]
    pub display_frame_select: FrameSelect,

    pub hblank_interval_free: bool,

    #[bits(1, default = ObjectMapType::D2)]
    pub obj_vram_mapping: ObjectMapType,

    pub forced_blank: bool,
    pub bg0_enable: bool,
    pub bg1_enable: bool,
    pub bg2_enable: bool,
    pub bg3_enable: bool,
    pub obj_enable: bool,
    pub window0_enable: bool,
    pub window1_enable: bool,
    pub obj_window_enable: bool,
}

impl LcdControl {
    /// Total available sprite render cycles per scanline
    pub fn sprite_render_cycles(&self) -> usize {
        if self.hblank_interval_free() {
            954
        } else {
            1210
        }
    }
}

#[bitenum]
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum ObjectMapType {
    #[fallback]
    /// two dimensional mapping in memory
    D2 = 0,
    /// one dimensional mapping in memory
    D1,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BgMode {
    Mode0 = 0,
    Mode1,
    Mode2,
    Mode3,
    Mode4,
    Mode5,
}

impl BgMode {
    const fn into_bits(self) -> u8 {
        self as u8
    }

    const fn from_bits(value: u8) -> Self {
        match value {
            0 => BgMode::Mode0,
            1 => BgMode::Mode1,
            2 => BgMode::Mode2,
            3 => BgMode::Mode3,
            4 => BgMode::Mode4,
            5 => BgMode::Mode5,
            _ => panic!("Invalid BgMode!"),
        }
    }

    pub fn is_bitmap(&self) -> bool {
        matches!(self, Self::Mode3 | Self::Mode4 | Self::Mode5)
    }
}

#[bitenum]
#[repr(u8)]
#[derive(Debug)]
pub enum FrameSelect {
    #[fallback]
    Page0 = 0,
    Page1 = 1,
}

#[gba_register(u16)]
pub struct LcdStatus {
    #[readonly]
    pub vblank_flag: bool,
    #[readonly]
    pub hblank_flag: bool,
    #[readonly]
    pub v_counter_flag: bool,

    pub vblank_irq_enable: bool,
    pub hblank_irq_enable: bool,
    pub vcounter_irq_enable: bool,

    #[bits(2)]
    __: u8, // unused

    #[bits(8)]
    pub vcount: u8,
}

#[gba_register(u16)]
pub struct VerticalCounter {
    #[readonly]
    pub scanline_count: u8,

    #[readonly]
    __: u8,
}

#[gba_register(u16)]
pub struct BgControl {
    #[bits(2)]
    pub bg_priority: u8,

    #[bits(2)]
    pub char_base_block: u8,

    #[bits(2)]
    __: u8,

    pub mosaic_enable: bool,

    #[bits(1, default = PaletteType::ColorDepth4Bit)]
    pub palette_type: PaletteType,

    #[bits(5)]
    pub screen_base_block: u8,

    #[bits(1, default = OverflowMode::Transparent)]
    /// overflow mode for affine layers
    pub display_area_overflow: OverflowMode,

    #[bits(2, default = ScreenSize::Layout0, from = ScreenSize::from_bits)]
    pub screen_size: ScreenSize,
}

#[bitenum]
#[derive(Debug)]
#[repr(u8)]
pub enum OverflowMode {
    #[fallback]
    Transparent = 0,
    Wraparound,
}

#[derive(Debug)]
pub enum ScreenSize {
    Layout0 = 0,
    Layout1,
    Layout2,
    Layout3,
}

impl ScreenSize {
    const fn into_bits(self) -> u8 {
        self as _
    }

    const fn from_bits(value: u8) -> Self {
        match value {
            0 => ScreenSize::Layout0,
            1 => ScreenSize::Layout1,
            2 => ScreenSize::Layout2,
            3 => ScreenSize::Layout3,
            _ => panic!("Invalid screen size value"),
        }
    }

    /// Dimensions of screen in pixels for affine backgrounds.
    pub fn affine_layout_size(&self) -> Vector2<u16> {
        match self {
            ScreenSize::Layout0 => Vector2::new(128, 128),
            ScreenSize::Layout1 => Vector2::new(256, 256),
            ScreenSize::Layout2 => Vector2::new(512, 512),
            ScreenSize::Layout3 => Vector2::new(1024, 1024),
        }
    }

    /// Dimensions of screen in tiles for affine backgrounds.
    pub fn affine_layout_tile_size(&self) -> Vector2<u8> {
        match self {
            ScreenSize::Layout0 => Vector2::new(16, 16),
            ScreenSize::Layout1 => Vector2::new(32, 32),
            ScreenSize::Layout2 => Vector2::new(64, 64),
            ScreenSize::Layout3 => Vector2::new(128, 128),
        }
    }

    /// Screen dimmensions in tiles as for normal backgrounds (non-affine).
    pub fn layout_tile_size(&self) -> Vector2<u8> {
        match self {
            // One screen block
            ScreenSize::Layout0 => Vector2::new(32, 32),
            // Two screen blocks layed out horizontally
            ScreenSize::Layout1 => Vector2::new(64, 32),
            // Two screen blocks layed out verically
            ScreenSize::Layout2 => Vector2::new(32, 64),
            // Four screen blocks layed out as a quad
            ScreenSize::Layout3 => Vector2::new(64, 64),
        }
    }

    /// Get number of screen blocks for the specified screen layout for normal backgrounds.
    pub fn get_block_count(&self) -> usize {
        match self {
            ScreenSize::Layout0 => 1,
            ScreenSize::Layout1 | ScreenSize::Layout2 => 2,
            ScreenSize::Layout3 => 4,
        }
    }
}

#[gba_register(u16)]
pub struct BgScroll {
    #[bits(9)]
    pub offset: u16,

    #[bits(7)]
    __: u8,
}

/// Reference point are similar to scroll registers for normal backgrounds.
/// Affine transformation matrix.
/// |dx, dmx|
/// |dy, dmy|
#[derive(Default, Clone)]
pub struct AffineParameters {
    pub reference_x: ReferencePoint,
    pub reference_y: ReferencePoint,
    pub dx: MatrixParameter,
    pub dmx: MatrixParameter,
    pub dy: MatrixParameter,
    pub dmy: MatrixParameter,
}

#[gba_register(u32)]
pub struct ReferencePoint {
    pub fraction: u8,

    #[bits(20)]
    pub integer: i32,

    #[bits(4)]
    __: u8,
}

impl ReferencePoint {
    /// sign extend 28-bit sign integer to a i32.
    pub fn get_int(&self) -> i32 {
        const SHIFT: u32 = i32::BITS - 28;
        ((self.into_bits() as i32) << SHIFT) >> SHIFT
    }
}

#[gba_register(u16)]
pub struct MatrixParameter {
    pub fraction: u8,

    #[bits(8)]
    pub integer: i8,
}

impl MatrixParameter {
    /// sign extended to i32
    pub fn get_int(&self) -> i32 {
        i32::from(self.into_bits() as i16)
    }
}
