mod arm_json_test_states;
mod common;
mod constants;
mod core;
pub mod decoder_tables;
mod opcode_tables;

pub use arm_json_test_states::*;
pub use constants::ARM7TDMI_CLOCK_RATE;
#[allow(unused_imports)]
pub use constants::{AccessCode, KindCode};
pub use core::{Arm7tdmi, InstructionType};
pub use decoder_tables::RingBuffer;

#[derive(Debug, Default, Clone)]
pub struct InstructionInfo {
    pub pc: u32,
    pub instr_type: InstructionType,
}
