use register_macros::gba_register;

pub struct Registers {
    pub lcd_control: LcdControl,
    pub lcd_status: LcdStatus,
    pub v_counter: VerticalCounter,
    pub bg_control_0: BgControl0,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            lcd_control: LcdControl::new(),
            lcd_status: LcdStatus::new(),
            v_counter: VerticalCounter::new(),
            bg_control_0: BgControl0::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum BgMode {
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

#[gba_register(u16)]
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
    pub vounter_irq_enable: bool,

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
pub struct BgControl0 {
    #[bits(2)]
    pub bg_priority: u8,

    #[bits(2)]
    pub char_base_block: u8,

    #[bits(2)]
    __: u8,

    pub mosaic: bool,
    pub palettes: bool,

    #[bits(5)]
    pub screen_base_block: u8,

    __: bool,

    #[bits(2)]
    pub screen_size: u8,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::io::HalfwordIo;

    #[test]
    fn test_reg() {
        let mut reg = LcdStatus::new();
        reg.write(0xFF, HalfwordIo::B0);
        assert_eq!(reg.read(HalfwordIo::B0), 0xF8);
    }
}
