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
    } else if (instruction & 0b1111_1011_1111) == 0b0001_0000_1001 {
        data_swap
    } else if (instruction & 0b1110_0000_1001) == 0b0000_0000_1001 && (instruction & 0b0110) != 0 {
        halfword_and_signed_data_transfer
    } else if (instruction & 0b1111_1011_0000) == 0b0001_0000_0000 {
        read_status_mrs
    } else if (instruction & 0b1101_1011_0000) == 0b0001_0010_0000 {
        write_status_msr
    } else if ((instruction & 0b1110_0000_0000) == 0b0010_0000_0000)
        || ((instruction & 0b1110_0000_0000) == 0 && (instruction & 0b1001) != 0b1001)
    {
        data_processing
    } else if (instruction & 0b1111_1100_1001) == 0b0000_0000_1001 {
        multiply
    } else if (instruction & 0b1111_1000_1001) == 0b0000_1000_1001 {
        multiply_long
    } else if (instruction & 0b1110_0000_0001) == 0b0110_0000_0001 {
        undefined_arm
    } else if (instruction & 0b1100_0000_0000) == 0b0100_0000_0000 {
        single_data_transfer
    } else if (instruction & 0b1110_0000_0000) == 0b1010_0000_0000 {
        branch_and_link
    } else if (instruction & 0b1110_0000_0000) == 0b1000_0000_0000 {
        block_data_transfer
    }else if (instruction & 0b1111_0000_0000) == 0b1111_0000_0000 {
        software_interrupt
    } 
    else {
        undefined_arm
    }
}
