mod apu;
mod arm;
mod bus;
mod config;
mod gamepak;
mod io;
mod keypad;
mod ppu;

use std::fs::{self, File};
use std::io::Read;
use std::path::Path;

use crate::arm::Arm7tdmi;
use crate::bus::Bus;
use GbaError::*;
use keypad::{KeyCode, KeypadInputType};

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

    #[inline]
    pub fn step(&mut self) {
        self.cpu.step(&mut self.bus);
    }

    pub fn keypad_set_input(&mut self, input_type: KeypadInputType, keycode: KeyCode) {
        let input_type = bool::from(input_type);

        match keycode {
            KeyCode::KeyA => self.bus.keypad.keypad_state.set_key_a(input_type),
            KeyCode::KeyB => self.bus.keypad.keypad_state.set_key_a(input_type),
            KeyCode::Select => self.bus.keypad.keypad_state.set_select(input_type),
            KeyCode::Start => self.bus.keypad.keypad_state.set_start(input_type),
            KeyCode::Right => self.bus.keypad.keypad_state.set_right(input_type),
            KeyCode::Left => self.bus.keypad.keypad_state.set_left(input_type),
            KeyCode::Up => self.bus.keypad.keypad_state.set_up(input_type),
            KeyCode::Down => self.bus.keypad.keypad_state.set_down(input_type),
            KeyCode::KeyR => self.bus.keypad.keypad_state.set_key_r(input_type),
            KeyCode::KeyL => self.bus.keypad.keypad_state.set_key_l(input_type),
        };
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

        bios_file.read_exact(&mut self.bus.bios_ram).map_err(|e| {
            BiosLoadFail(format!(
                "Failed to load bios at: {:?}, {}",
                bios_path.as_ref(),
                e.to_string()
            ))
        })?;

        Ok(())
    }

    fn load_gamepak<P: AsRef<Path>>(&mut self, gamepak_path: P) -> Result<(), GbaError> {
        let buffer = fs::read(&gamepak_path).map_err(|e| {
            GamepakLoadFail(format!(
                "Failed to load gamepak at: {:?}, {}",
                gamepak_path.as_ref(),
                e.to_string()
            ))
        })?;

        self.bus.gamepak.rom = buffer.into_boxed_slice();
        Ok(())
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
