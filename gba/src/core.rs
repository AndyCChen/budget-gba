use crate::arm::Arm7tdmi;
use crate::bus::Bus;
use crate::common::DisplayBuffer;
use crate::config::GbaCoreConfig;
use crate::keypad::{KeyCode, KeypadInputType};
use GbaError::*;
use std::fmt::Display;
use std::fs::{self, File};
use std::io::Read;
use std::path::Path;

pub struct GbaCore {
    cpu: Arm7tdmi<Bus>,
    bus: Bus,
}

impl Default for GbaCore {
    fn default() -> Self {
        Self::new()
    }
}

impl GbaCore {
    pub fn new() -> Self {
        Self {
            cpu: Arm7tdmi::new(),
            bus: Bus::new(),
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.bus.reset();
    }

    #[inline]
    pub fn step(&mut self) {
        self.cpu.step(&mut self.bus);
    }

    pub fn cpu_pipeline_fill(&mut self) {
        self.cpu.pipeline_refill_arm(&mut self.bus);
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

    pub fn toggle_cpu_log(&mut self, toggle: bool) {
        self.cpu.instruction_log_enable = toggle;
    }

    pub fn print_cpu_log(&mut self) {
        self.cpu.print_log();
    }

    pub fn get_display_buffer(&self) -> &DisplayBuffer {
        &self.bus.ppu.display_buffer
    }

    #[inline]
    pub fn is_frame_complete(&mut self) -> bool {
        self.bus.ppu.is_frame_complete()
    }

    fn load_bios<P: AsRef<Path>>(&mut self, bios_path: P) -> Result<(), GbaError> {
        let mut bios_file = File::open(&bios_path).map_err(|e| {
            BiosLoadFail(format!(
                "Failed to load bios at: {:?}, {e}",
                bios_path.as_ref(),
            ))
        })?;

        bios_file.read_exact(&mut self.bus.bios_ram).map_err(|e| {
            BiosLoadFail(format!(
                "Failed to load bios at: {:?}, {e}",
                bios_path.as_ref()
            ))
        })?;
        Ok(())
    }

    fn load_gamepak<P: AsRef<Path>>(&mut self, gamepak_path: P) -> Result<(), GbaError> {
        let buffer = fs::read(&gamepak_path).map_err(|e| {
            GamepakLoadFail(format!(
                "Failed to load gamepak at: {:?}, {e}",
                gamepak_path.as_ref(),
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

impl Display for GbaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GamepakLoadFail(msg) => write!(f, "{msg}"),
            BiosLoadFail(msg) => write!(f, "{msg}"),
        }
    }
}
