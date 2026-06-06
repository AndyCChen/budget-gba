mod apu;
mod arm;
mod bus;
mod config;
mod gamepak;
mod io;
mod ppu;

use std::fs::{self, File};
use std::io::Read;
use std::path::Path;

use crate::arm::Arm7tdmi;
use crate::bus::Bus;
use GbaError::*;

pub use config::GbaCoreConfig;

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

    pub fn load_config(&mut self, config: &GbaCoreConfig) -> Result<(), GbaError> {
        self.load_bios(&config.bios_path)?;
        if let Some(gamepak_path) = &config.gamepak_path {
            self.load_gamepak(gamepak_path)?;
        }

        Ok(())
    }

    fn load_bios<P: AsRef<Path>>(&mut self, bios_path: P) -> Result<(), GbaError> {
        let mut bios_file = File::open(&bios_path).map_err(|e| {
            BiosLoadFail(format!(
                "Failed to load bios at: {:?}, {}",
                bios_path.as_ref(),
                e.to_string()
            ))
        })?;

        match bios_file.read_exact(&mut self.bus.bios_ram) {
            Ok(_) => Ok(()),
            Err(e) => Err(BiosLoadFail(format!(
                "Failed to load bios at: {:?}, {}",
                bios_path.as_ref(),
                e.to_string()
            ))),
        }
    }

    fn load_gamepak<P: AsRef<Path>>(&mut self, gamepak_path: P) -> Result<(), GbaError> {
        match fs::read(&gamepak_path) {
            Ok(buffer) => {
                self.bus.gamepak.rom = buffer.into_boxed_slice();
                Ok(())
            }
            Err(e) => Err(GamepakLoadFail(format!(
                "Failed to load gamepak at: {:?}, {}",
                gamepak_path.as_ref(),
                e.to_string()
            ))),
        }
    }
}

pub enum GbaError {
    GamepakLoadFail(String),
    BiosLoadFail(String),
}

impl ToString for GbaError {
    fn to_string(&self) -> String {
        match self {
            GamepakLoadFail(msg) => msg.clone(),
            BiosLoadFail(msg) => msg.clone(),
        }
    }
}
