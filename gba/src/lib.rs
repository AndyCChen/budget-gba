mod arm;
mod audio;
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
}
