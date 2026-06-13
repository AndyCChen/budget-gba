use bitfield_struct::bitenum;
use register_macros::gba_register;

#[derive(Debug)]
pub enum KeyCode {
    KeyA,
    KeyB,
    Select,
    Start,
    Right,
    Left,
    Up,
    Down,
    KeyR,
    KeyL,
}

pub enum KeypadInputType {
    Pressed,
    Released,
}

impl From<KeypadInputType> for bool {
    fn from(value: KeypadInputType) -> Self {
        match value {
            KeypadInputType::Pressed => false,
            KeypadInputType::Released => true,
        }
    }
}

pub struct Keypad {
    pub keypad_state: KeypadStatus,
    pub interrupt_control: KeypadInterruptControl,
}

impl Keypad {
    pub fn new() -> Self {
        Self {
            keypad_state: KeypadStatus::default(),
            interrupt_control: KeypadInterruptControl::default(),
        }
    }

    pub fn reset(&mut self) {
        self.keypad_state = KeypadStatus::default();
        self.interrupt_control = KeypadInterruptControl::default();
    }
}

#[gba_register(u16)]
pub struct KeypadStatus {
    #[readonly]
    #[bits(1, default = true)]
    pub key_a: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub key_b: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub select: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub start: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub right: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub left: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub up: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub down: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub key_r: bool,

    #[readonly]
    #[bits(1, default = true)]
    pub key_l: bool,

    #[bits(6)]
    #[readonly]
    __: u8,
}

#[gba_register(u16)]
pub struct KeypadInterruptControl {
    pub key_a: bool,
    pub key_b: bool,
    pub select: bool,
    pub start: bool,
    pub right: bool,
    pub left: bool,
    pub up: bool,
    pub down: bool,
    pub key_r: bool,
    pub key_l: bool,

    #[bits(4)]
    __: u8,

    irq_enable: bool,

    #[bits(1, default = KeypadIrqCondition::LogicalOr)]
    irq_condition: KeypadIrqCondition,
}

#[bitenum]
#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum KeypadIrqCondition {
    #[fallback]
    LogicalOr = 0,
    LogicalAnd = 1,
}
