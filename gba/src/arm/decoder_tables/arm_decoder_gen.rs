use crate::arm::decoder_tables::arm_decoder::*;
use crate::arm::opcode_tables::ARM_TABLE_SIZE;

pub type ArmDecoder = fn(u32) -> ArmInstructionInfo;

pub const fn generate_arm_decoder_table() -> [ArmDecoder; ARM_TABLE_SIZE] {
    let mut arm_table: [ArmDecoder; ARM_TABLE_SIZE] = [undefined_arm; ARM_TABLE_SIZE];

    let mut i = 0;
    while i < ARM_TABLE_SIZE {
        arm_table[i] = generate_arm_decoder(i);
        i += 1;
    }

    arm_table
}

const fn generate_arm_decoder(instruction: usize) -> ArmDecoder {
    if instruction == 0b0001_0010_0001 {
        branch_and_exchange
    } else if (instruction & 0b1110_0000_0000) == 0b1010_0000_0000 {
        branch_and_link
    } else if ((instruction & 0b1110_0000_0000) == 0b0010_0000_0000)
        || ((instruction & 0b1110_0000_0000) == 0 && (instruction & 0b1001) != 0b1001)
    {
        data_processing
    } else {
        undefined_arm
    }
}
