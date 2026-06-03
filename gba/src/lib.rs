mod apu;
mod arm;
mod bus;
mod gamepak;
mod io;
mod ppu;

use crate::arm::Arm7tdmi;
use crate::bus::Bus;

pub struct GbaCore {
    cpu: Arm7tdmi<Bus>,
    bus: Bus,
}

impl GbaCore {
    pub fn new() -> Self {
        let mut bus = Bus::new();
        let cpu = Arm7tdmi::new(&mut bus);

        Self { cpu, bus }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.bus.reset();
    }

    pub fn step(&mut self) {
        self.cpu.step(&mut self.bus);
    }
}
