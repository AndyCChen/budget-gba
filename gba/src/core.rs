use crate::arm::decoder_tables::{decode_arm, decode_thumb};
use crate::arm::{Arm7tdmi, InstructionInfo, InstructionType, RingBuffer};
use crate::bus::{Bus, BusInterface};
use crate::common::DisplayBuffer;
use crate::config::GbaCoreConfig;
use crate::halt_control::PowerMode;
use crate::keypad::{KeyCode, KeypadInputType};
use GbaError::*;
use std::fmt::Display;
use std::fs;
use std::io::Read;
use std::path::Path;

pub struct GbaCore {
    cpu: Arm7tdmi<Bus>,
    bus: Bus,
    instruction_buffer: RingBuffer<InstructionInfo>,
    enable_instruction_log: bool,
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
            instruction_buffer: RingBuffer::new(32),
            enable_instruction_log: false,
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.bus.reset();
        self.instruction_buffer.clear();
        self.enable_instruction_log = false;
    }

    #[inline]
    pub fn is_frame_complete(&mut self) -> bool {
        self.bus.ppu.is_frame_complete()
    }

    #[inline]
    pub fn step(&mut self) {
        // handle interrupt if one is requested and interupts are enabled in cpsr
        if !self.cpu.status.cpsr.i() && self.bus.interrupt.interrupt_requested() {
            self.cpu.do_interrupt(&mut self.bus);
        }

        match self.bus.halt_controller.state {
            None => {
                if self.enable_instruction_log {
                    self.cpu.record_instruction(&mut self.instruction_buffer);
                }

                self.cpu.step(&mut self.bus)
            }
            Some(PowerMode::Halt) => {
                while !self.bus.ppu.is_frame_complete_retain() {
                    self.bus.i_cycle();
                    if self.bus.interrupt.interrupt_raised() {
                        self.bus.halt_controller.state = None; // "wake up" cpu when interrupt is raised, regardless of if IME is set 
                        break;
                    }
                }
            }
            // Stop mode suspends most components the system, only wakes up when a keypad interrupt is raised, I think...
            Some(PowerMode::Stop) => {
                println!("GBA STOP MODE");
                if self.bus.interrupt.interrupt_raised() {
                    self.bus.halt_controller.state = None
                }
            }
        }
    }

    pub fn cpu_pipeline_fill(&mut self) {
        self.cpu.pipeline_refill_arm(&mut self.bus);
    }

    pub fn toggle_instruction_log(&mut self, flag: bool) {
        self.enable_instruction_log = flag;
    }

    pub fn print_cpu_log(&self) {
        let Some(InstructionInfo { pc, instr_type }) = self.instruction_buffer.iter().next() else {
            return;
        };

        let (asm_string, pc) = match instr_type {
            InstructionType::Arm(opcode) => {
                (decode_arm(*opcode).to_asm_string(*pc), pc.wrapping_sub(8))
            }
            InstructionType::Thumb(opcode) => (
                decode_thumb(*opcode as u16).to_asm_string(*pc),
                pc.wrapping_sub(4),
            ),
        };

        println!("0x{pc:08X}    {asm_string}");
    }

    pub fn get_display_buffer(&self) -> &DisplayBuffer {
        &self.bus.ppu.display_buffer
    }

    pub fn keypad_set_input(&mut self, input_type: KeypadInputType, keycode: KeyCode) {
        let input_type = bool::from(input_type);

        match keycode {
            KeyCode::KeyA => self.bus.keypad.keypad_state.set_key_a(input_type),
            KeyCode::KeyB => self.bus.keypad.keypad_state.set_key_b(input_type),
            KeyCode::Select => self.bus.keypad.keypad_state.set_select(input_type),
            KeyCode::Start => self.bus.keypad.keypad_state.set_start(input_type),
            KeyCode::Right => self.bus.keypad.keypad_state.set_right(input_type),
            KeyCode::Left => self.bus.keypad.keypad_state.set_left(input_type),
            KeyCode::Up => self.bus.keypad.keypad_state.set_up(input_type),
            KeyCode::Down => self.bus.keypad.keypad_state.set_down(input_type),
            KeyCode::KeyR => self.bus.keypad.keypad_state.set_key_r(input_type),
            KeyCode::KeyL => self.bus.keypad.keypad_state.set_key_l(input_type),
        };

        if self.bus.keypad.irq_requested() {
            self.bus.interrupt.interrupt_flags.set_keypad(true);
        }
    }

    pub fn load_config(&mut self, config: &GbaCoreConfig) -> Result<(), GbaError> {
        self.load_bios(&config.bios_path)?;
        if let Some(gamepak_path) = &config.gamepak_path {
            self.load_gamepak(gamepak_path)?;
        }

        Ok(())
    }

    fn load_bios<P: AsRef<Path>>(&mut self, bios_path: P) -> Result<(), GbaError> {
        let mut bios_file = fs::File::open(&bios_path).map_err(|e| {
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
