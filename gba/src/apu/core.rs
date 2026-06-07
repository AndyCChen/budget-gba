use crate::apu::Registers;

pub struct Apu {
    pub registers: Registers,
}

impl Apu {
    pub fn new() -> Self {
        Self {
            registers: Registers::new(),
        }
    }

    pub fn reset(&mut self) {
        *self = Self::new();
    }
}
