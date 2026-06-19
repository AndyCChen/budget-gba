use bitfield_struct::bitenum;
use register_macros::gba_register;

pub struct HaltController {
    pub halt_control: HaltControl,
    pub state: Option<PowerMode>,
}

impl HaltController {
    pub fn new() -> Self {
        Self {
            halt_control: HaltControl::default(),
            state: None,
        }
    }
}

#[gba_register(u8)]
pub struct HaltControl {
    #[bits(7)]
    __: u8,

    #[bits(1, default = PowerMode::Halt)]
    pub power_down_mode: PowerMode,
}

#[bitenum]
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum PowerMode {
    #[fallback]
    Halt = 0,
    Stop = 1,
}
