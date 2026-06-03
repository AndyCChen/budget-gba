mod arm_json_test_states;
mod constants;
mod core;
pub mod decoder_tables;
mod opcode_tables;

pub use arm_json_test_states::*;
pub use constants::ARM7TDMI_CLOCK_RATE;
#[allow(unused_imports)]
pub use constants::access_code;
pub use constants::kind_code;
pub use core::{Arm7tdmi, CpuInstruction};
pub use opcode_tables::arm_data_op;
pub use opcode_tables::{
    ARM_TABLE_SIZE, ArmHandler, THUMB_TABLE_SIZE, ThumbHandler, generate_arm_table,
    generate_thumb_table,
};
