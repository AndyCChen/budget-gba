use bitfield_struct::bitfield;
use utils::{ReadIo16, WriteIo16};

pub struct Registers {
    pub lcd_control: LcdControl,
    pub lcd_status: LcdStatus,
    pub v_counter: VerticalCounter,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            lcd_control: LcdControl::new(),
            lcd_status: LcdStatus::new(),
            v_counter: VerticalCounter::new(),
        }
    }
}

pub enum HalfwordIo {
    B1, // 1st byte
    B2, // 2nd byte
}

pub enum WordIo {
    B1, // 1st byte
    B2, // etc...
    B3,
    B4,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
enum BgMode {
    Mode0,
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

pub trait ReadIoHalfWord {
    fn read(&self, byte_select: HalfwordIo) -> u8;
}

pub trait ReadIoWord {
    fn read(&self, byte_select: WordIo) -> u8;
}

pub trait WriteIoHalfword {
    fn write(&mut self, value: u8, byte_select: HalfwordIo);
}

pub trait WriteIoWord {
    fn write(&mut self, value: u8, byte_select: HalfwordIo);
}

#[bitfield(u16)]
#[derive(ReadIo16, WriteIo16)]
pub struct LcdControl {
    #[bits(3, default = BgMode::Mode0, from = BgMode::from_bits)]
    pub bg_mode: BgMode,
    pub cgb_mode: bool, // only set by bios
    pub display_frame_select: bool,
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

#[bitfield(u16)]
#[derive(ReadIo16)]
pub struct LcdStatus {
    pub vblank_flag: bool,
    pub hblank_flag: bool,
    pub v_counter_flag: bool,

    pub vblank_irq_enable: bool,
    pub hblank_irq_enable: bool,
    pub vounter_irq_enable: bool,

    #[bits(2)]
    __: u8, // unused

    #[bits(8)]
    pub vcount: u8,
}

impl WriteIoHalfword for LcdStatus {
    fn write(&mut self, value: u8, byte_select: HalfwordIo) {
        let value = u16::from(value);
        let v = self.into_bits();
        match byte_select {
            HalfwordIo::B1 => *self = Self::from_bits((v & 0xFF00) | (value & !7)), // bits 0..2 are read only
            HalfwordIo::B2 => *self = Self::from_bits((v & 0x00FF) | (value << 8)),
        }
    }
}

#[bitfield(u16)]
#[derive(ReadIo16)]
pub struct VerticalCounter {
    scanline_count: u8,
    __: u8,
}
