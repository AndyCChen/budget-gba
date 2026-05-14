mod arm_decoder;
mod arm_decoder_string;
mod arm_decoder_gen;

use crate::arm::opcode_tables::ARM_TABLE_SIZE;

use arm_decoder_gen::ArmDecoder;
use arm_decoder_gen::generate_arm_decoder_table;

pub static ARM_DECODER: [ArmDecoder; ARM_TABLE_SIZE] = generate_arm_decoder_table();
