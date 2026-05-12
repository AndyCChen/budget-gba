use crate::io::registers::*;
use bitfield_struct::bitfield;
use register_macros::ReadIo32;

pub struct Registers {
    pub waitstate_control: WaitStateControl,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            waitstate_control: WaitStateControl::new(),
        }
    }
}

#[derive(Debug)]
#[repr(u8)]
enum GamePakType {
    Gba = 0,
    Cgba = 1,
}

impl GamePakType {
    const fn into_bits(self) -> u8 {
        self as u8
    }

    const fn from_bits(value: u8) -> Self {
        match value {
            0 => GamePakType::Gba,
            1 => GamePakType::Cgba,
            _ => panic!("Invalid GamePak Type!"),
        }
    }
}

#[bitfield(u32)]
#[derive(ReadIo32)]
pub struct WaitStateControl {
    #[bits(2)]
    pub sram_wait_control: u8,

    #[bits(2)]
    pub wait_state_0_first: u8,
    pub wait_state_0_second: bool,

    #[bits(2)]
    pub wait_state_1_first: u8,
    pub wait_state_1_second: bool,

    #[bits(2)]
    pub wait_state_2_first: u8,
    pub wait_state_2_second: bool,

    #[bits(2)]
    pub phi_terminal_output: u8, // should always be 0 (disabled)? I'm not too sure what this does...

    __: bool, // unused

    pub gamepak_prefetch_enable: bool,

    #[bits(1, default = GamePakType::Gba, from = GamePakType::from_bits)]
    pub gamepak_type: GamePakType,

    __: u16, // unused
}

impl WriteIoWord for WaitStateControl {
    fn write(&mut self, value: u8, byte_select: WordIo) {
        let value = u32::from(value);
        let v = self.into_bits();

        *self = match byte_select {
            WordIo::B0 => Self::from_bits((v & 0x0000_FF00) | value),
            WordIo::B1 => Self::from_bits((v & 0x0000_00FF) | ((value & !0x80) << 8)), // bit 15 is read only
            WordIo::B2 => Self::from_bits((v & 0xFF00_FFFF) | (value << 16)),
            WordIo::B3 => Self::from_bits((v & 0x00FF_FFFF) | (value << 24)),
        }
    }
}
