use crate::arm::AccessCode;
use crate::gamepak::Registers;

pub struct GamePak {
    pub rom: Box<[u8]>,
    // There does not seem to be a reliable way to detect presense of sram on cartridges outside of manually having
    // a database of cartridge data so ignore this for now.
    pub _sram: Box<[u8]>,
    pub registers: Registers,
}

impl GamePak {
    pub fn new() -> Self {
        Self {
            rom: vec![].into_boxed_slice(),
            _sram: vec![].into_boxed_slice(),
            registers: Registers::new(),
        }
    }

    pub fn reset(&mut self) {
        // self.rom.fill(0);
        // self.sram.fill(0);
        self.registers = Registers::new();
    }

    pub fn get_wait_states(&self, access: AccessType, region: GamepakRegion) -> u8 {
        match (region, access) {
            (GamepakRegion::Region8_9, AccessType::First) => {
                match self.registers.waitstate_control.wait_state_0_first() {
                    0 => 4,
                    1 => 3,
                    2 => 2,
                    3 => 8,
                    _ => panic!("Wait state must be 0-3!"),
                }
            }
            (GamepakRegion::Region8_9, AccessType::Second) => {
                match self.registers.waitstate_control.wait_state_0_second() {
                    false => 2,
                    true => 1,
                }
            }

            (GamepakRegion::Region10_11, AccessType::First) => {
                match self.registers.waitstate_control.wait_state_1_first() {
                    0 => 4,
                    1 => 3,
                    2 => 2,
                    3 => 8,
                    _ => panic!("Wait state must be 0-3!"),
                }
            }
            (GamepakRegion::Region10_11, AccessType::Second) => {
                match self.registers.waitstate_control.wait_state_1_second() {
                    false => 4,
                    true => 1,
                }
            }

            (GamepakRegion::Region12_13, AccessType::First) => {
                match self.registers.waitstate_control.wait_state_2_first() {
                    0 => 4,
                    1 => 3,
                    2 => 2,
                    3 => 8,
                    _ => panic!("Wait state must be 0-3!"),
                }
            }
            (GamepakRegion::Region12_13, AccessType::Second) => {
                match self.registers.waitstate_control.wait_state_2_second() {
                    false => 8,
                    true => 1,
                }
            }
        }
    }
}

pub enum GamepakRegion {
    Region8_9,
    Region10_11,
    Region12_13,
}

pub enum AccessType {
    First,
    Second,
}

impl TryFrom<AccessCode> for AccessType {
    type Error = &'static str;

    fn try_from(mut value: AccessCode) -> Result<Self, Self::Error> {
        value = value & (AccessCode::NONSEQUENTIAL | AccessCode:: SEQUENTIAL);
        
        // Ignore the other bitflags for now and only care about whether it is sequential or nonsequential.
        if AccessCode::NONSEQUENTIAL.contains(value) {
            Ok(AccessType::First)
        } else if AccessCode::SEQUENTIAL.contains(value) {
            Ok(AccessType::Second)
        } else {
            Err("Access type must be sequential or nonsequential!")
        }
    }
}
