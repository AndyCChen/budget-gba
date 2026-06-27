use bitfield_struct::bitenum;
use register_macros::gba_register;

pub struct Registers {
    pub lcd_control: LcdControl,
    pub lcd_status: LcdStatus,
    pub v_counter: VerticalCounter,
    pub bg0_control: BgControl,
    pub bg1_control: BgControl,
    pub bg2_control: BgControl,
    pub bg3_control: BgControl,
    pub bg0_scroll_x: BgScroll,
    pub bg0_scroll_y: BgScroll,
    pub bg1_scroll_x: BgScroll,
    pub bg1_scroll_y: BgScroll,
    pub bg2_scroll_x: BgScroll,
    pub bg2_scroll_y: BgScroll,
    pub bg3_scroll_x: BgScroll,
    pub bg3_scroll_y: BgScroll,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            lcd_control: LcdControl::default(),
            lcd_status: LcdStatus::default(),
            v_counter: VerticalCounter::default(),
            bg0_control: BgControl::default(),
            bg1_control: BgControl::default(),
            bg2_control: BgControl::default(),
            bg3_control: BgControl::default(),
            bg0_scroll_x: BgScroll::default(),
            bg0_scroll_y: BgScroll::default(),
            bg1_scroll_x: BgScroll::default(),
            bg1_scroll_y: BgScroll::default(),
            bg2_scroll_x: BgScroll::default(),
            bg2_scroll_y: BgScroll::default(),
            bg3_scroll_x: BgScroll::default(),
            bg3_scroll_y: BgScroll::default(),
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
    pub obj_vram_mapping: bool,
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
    pub palettes: PaletteType,

    #[bits(5)]
    pub screen_base_block: u8,

    __: bool,

    #[bits(2, default = ScreenSize::Layout0, from = ScreenSize::from_bits)]
    pub screen_size: ScreenSize,
}

#[bitenum]
#[repr(u8)]
#[derive(Debug)]
pub enum PaletteType {
    /// 16 colors / 16 palettes
    #[fallback]
    ColorDepth4Bit = 0,
    /// 256 colors / 1 palette
    ColorDepth8Bit,
}

#[derive(Debug)]
pub enum ScreenSize {
    /// One screen block
    Layout0 = 0,
    /// Two screen blocks layed out horizontally
    Layout1,
    /// Two screen blocks layed out verically
    Layout2,
    /// Four screen blocks layed out as a quad
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
            _ => panic!("Invallid screen size value"),
        }
    }

    /// Screen tile dimmensions in (x, y) format for text mode.
    pub fn layout_tile_size(&self) -> (u8, u8) {
        match self {
            ScreenSize::Layout0 => (32, 32),
            ScreenSize::Layout1 => (64, 32),
            ScreenSize::Layout2 => (32, 64),
            ScreenSize::Layout3 => (64, 64),
        }
    }

    /// Get number of screen blocks for the specified screen layout
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
    pub offset: usize,

    #[bits(7)]
    __: u8,
}
