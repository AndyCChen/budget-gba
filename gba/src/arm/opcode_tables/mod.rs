mod arm_handlers;
mod arm_table_gen;
mod common;
mod thumb_handlers;
mod thumb_table_gen;

use arm_table_gen::*;
use thumb_table_gen::*;

pub use arm_table_gen::ARM_TABLE_SIZE;
pub use common::reg_constant;
pub use common::{
    arithmetic::{ASR, LSL, LSR, ROR},
    arm_data_op, to_negative,
};
pub use thumb_table_gen::THUMB_TABLE_SIZE;

pub static ARM_TABLE: [ArmHandler; ARM_TABLE_SIZE] = generate_arm_table();
pub static THUMB_TABLE: [ThumbHandler; THUMB_TABLE_SIZE] = generate_thumb_table();
