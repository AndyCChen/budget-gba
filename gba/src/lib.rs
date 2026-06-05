mod apu;
mod arm;
mod bus;
mod gamepak;
mod io;
mod ppu;

use std::fs::{self, File};
use std::io::Read;
use std::path::Path;

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

    pub fn load_bios<P: AsRef<Path>>(&mut self, bios_path: P) {
        let mut bios_file = File::open(bios_path).expect("Failed to load bios!");
        match bios_file.read_exact(&mut self.bus.bios_ram) {
            Ok(_) => (),
            Err(e) => {
                panic!("{}", e.to_string())
            }
        }
    }

    pub fn load_gamepak<P: AsRef<Path>>(&mut self, gamepak_path: P) {
        match fs::read(gamepak_path) {
            Ok(buffer) => self.bus.gamepak.rom = buffer.into_boxed_slice(),
            Err(e) => panic!("{}", e.to_string()),
        }
    }
}
