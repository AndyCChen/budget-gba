use bitfield_struct::bitfield;
use register_macros::gba_register;

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

#[gba_register(u32)]
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
    /// should always be 0 (disabled)? I'm not too sure what this does...
    pub phi_terminal_output: u8,

    __: bool, // unused

    #[bits(1, default = true, access = RO)]
    pub gamepak_prefetch_enable: bool,

    #[bits(1, default = GamePakType::Gba, from = GamePakType::from_bits)]
    pub gamepak_type: GamePakType,

    __: u16, // unused
}

#[cfg(test)]
mod test {
    use super::WaitStateControl;
    use crate::io::WordIo;

    #[test]
    fn test_regsiter() {
        let mut reg = WaitStateControl::default();
        reg.write(0xBF, WordIo::B1);
        println!("{reg:#?}");
    }
}
