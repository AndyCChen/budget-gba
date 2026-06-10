mod arm_decoder;
mod arm_decoder_gen;
mod arm_decoder_string;
mod decoder_ringbuffer;
mod thumb_decoder;
mod thumb_decoder_gen;
mod thumb_decoder_string;

use crate::arm::opcode_tables::{ARM_TABLE_SIZE, THUMB_TABLE_SIZE};

use arm_decoder::ArmInstructionInfo;
use arm_decoder_gen::{ArmDecoder, generate_arm_decoder_table};
pub use decoder_ringbuffer::*;
use thumb_decoder::ThumbInstruction;
use thumb_decoder_gen::{ThumbDecoder, generate_thumb_decoder_table};

static ARM_DECODER: [ArmDecoder; ARM_TABLE_SIZE] = generate_arm_decoder_table();
static THUMB_DECODER: [ThumbDecoder; THUMB_TABLE_SIZE] = generate_thumb_decoder_table();

pub fn decode_arm(opcode: u32) -> ArmInstructionInfo {
    let arm_table_hash = ((opcode & 0x0FF00000) >> 16) | ((opcode & 0xF0) >> 4);
    ARM_DECODER[arm_table_hash as usize](opcode)
}

pub fn decode_thumb(opcode: u16) -> ThumbInstruction {
    let thumb_table_hash = (opcode >> 6) & 0x3FF;
    THUMB_DECODER[thumb_table_hash as usize](opcode)
}
