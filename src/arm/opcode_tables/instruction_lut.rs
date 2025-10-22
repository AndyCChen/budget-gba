use crate::arm::opcode_tables::arm_table_gen::*;
use crate::arm::opcode_tables::thumb_table_gen::*;

pub static ARM_TABLE: [ArmHandler; ARM_TABLE_SIZE] = generate_arm_table();
pub static THUMB_TABLE: [ThumbHandler; THUMB_TABLE_SIZE] = generate_thumb_table();
