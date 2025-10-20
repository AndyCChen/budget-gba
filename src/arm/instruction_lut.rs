use crate::arm::core::Arm7tdmi;

type ArmHandler = fn(&mut Arm7tdmi, u32);

const ARM_TABLE_LENGTH: usize = 0x1000;

pub static ARM_TABLE: [ArmHandler; ARM_TABLE_LENGTH] = generate_arm_table();

const fn generate_arm_table() -> [ArmHandler; ARM_TABLE_LENGTH] {
    use crate::arm::arm_handlers::*;

    let mut arm_table: [ArmHandler; ARM_TABLE_LENGTH] = [undefined_arm; ARM_TABLE_LENGTH];

    let mut i = 0;

    while i < ARM_TABLE_LENGTH {
        arm_table[i] = generate_arm_instruction(i);
        i += 1;
    }

    arm_table
}

const fn generate_arm_instruction(instruction: usize) -> ArmHandler {
    use crate::arm::arm_handlers::*;
    use crate::{_data_processing_inner, data_processing};

    match instruction >> 10 {
        0b00 => {
            if instruction == 0b0001_0010_0001 {
                branch_and_exchange
            }
            else if (instruction & 0b1111_1011_1111) == 0b0001_0000_1001 {
                let byte_quantity = (instruction >> 6) & 1 == 1;
                match byte_quantity {
                    true => data_swap::<true>,
                    false => data_swap::<false>,
                }
            }  
            else if (instruction & 0b1110_0000_1001) == 0b0000_0000_1001 && (instruction & 0b0110) != 0 {
                let is_immediate = (instruction >> 6) & 1 == 1;

                match is_immediate {
                    true => generate_arm_halfword_transfer::<true>(instruction),
                    false => generate_arm_halfword_transfer::<false>(instruction)
                }
            }  
            else if (instruction & 0b0001_1011_0000) == 0b0001_0000_0000 {
                let is_source_spsr = (instruction & 0b0000_0100_0000) != 0;

                match is_source_spsr {
                    true => read_status_mrs::<true>,
                    false => read_status_mrs::<false>,
                }
            } else if (instruction & 0b0001_1011_0000) == 0b0001_0010_0000 {
                let is_immediate = (instruction & 0b0010_0000_0000) != 0;
                let is_source_spsr = (instruction & 0b0000_0100_0000) != 0;

                match (is_immediate, is_source_spsr) {
                    (true, true) => write_status_msr::<true, true>,
                    (true, false) => write_status_msr::<true, false>,
                    (false, true) => write_status_msr::<false, true>,
                    (false, false) => write_status_msr::<false, false>,
                }
            }
            // data proc immediate mode
            else if (instruction & 0b0010_0000_0000) != 0 {
                let data_opcode: u8 = ((instruction >> 5) & 0xF) as u8;
                let shift_field: u8 = (instruction & 0xF) as u8;
                let set_condition = (instruction & 0b0000_0001_0000) != 0;

                match set_condition {
                    true => data_processing!(true, data_opcode, true, shift_field),
                    false => data_processing!(true, data_opcode, false, shift_field),
                }
            }
            // data proc non-immediate mode
            else if (instruction & 0b0010_0000_0000) == 0 && (instruction & 0b1001) != 0b1001 {
                let data_opcode: u8 = ((instruction >> 5) & 0xF) as u8;
                let shift_field: u8 = (instruction & 0xF) as u8;
                let set_condition = (instruction & 0b0000_0001_0000) != 0;

                match set_condition {
                    true => data_processing!(false, data_opcode, true, shift_field),
                    false => data_processing!(false, data_opcode, false, shift_field),
                }
            }
            else if (instruction & 0b1111_1100_1001) == 0b0000_0000_1001 {
                let accumulate = (instruction >> 5) & 1 == 1;
                let set_condition = (instruction >> 4) & 1 == 1;

                match (accumulate, set_condition) {
                    (true, true) => multiply::<true, true>,
                    (true, false) => multiply::<true, false>,
                    (false, true) => multiply::<false, true>,
                    (false, false) => multiply::<false, false>,
                }
            } else if (instruction & 0b1111_1000_1001) == 0b0000_1000_1001 {
                let signed = (instruction >> 6) & 1 == 1;
                let accumulate = (instruction >> 5) & 1 == 1;
                let set_condition = (instruction >> 4) & 1 == 1;

                match (signed, accumulate, set_condition) {
                    (true, true, true) => multiply_long::<true, true, true>,
                    (true, true, false) => multiply_long::<true, true, false>,
                    (true, false, true) => multiply_long::<true, false, true>,
                    (true, false, false) => multiply_long::<true, false, false>,
                    (false, true, true) => multiply_long::<false, true, true>,
                    (false, true, false) => multiply_long::<false, true, false>,
                    (false, false, true) => multiply_long::<false, false, true>,
                    (false, false, false) => multiply_long::<false, false, false>,
                }
            }
            else {
                undefined_arm
            }
        }
        0b01 => {
            if (instruction & 0b1110_0000_0001) == 0b0110_0000_0001 {
                undefined_arm
            } else  {
                // 01IP_UBWL_****

                let is_immediate = (instruction >> 9) & 1 == 0;
                let pre_indexing = (instruction >> 8) & 1 == 1;
                let increment = (instruction >> 7) & 1 == 1;
                let byte_quantity = (instruction >> 6) & 1 == 1;
                let write_back = (instruction >> 5) & 1 == 1;
                let load = (instruction >> 4) & 1 == 1;

                match (is_immediate, pre_indexing, increment, byte_quantity, write_back, load) {
                    (true, true, true, true, true, true) => single_data_transfer::<true, true, true, true, true, true>,
                    (true, true, true, true, true, false) => single_data_transfer::<true, true, true, true, true, false>,
                    (true, true, true, true, false, true) => single_data_transfer::<true, true, true, true, false, true>,
                    (true, true, true, true, false, false) => single_data_transfer::<true, true, true, true, false, false>,
                    (true, true, true, false, true, true) => single_data_transfer::<true, true, true, false, true, true>,
                    (true, true, true, false, true, false) => single_data_transfer::<true, true, true, false, true, false>,
                    (true, true, true, false, false, true) => single_data_transfer::<true, true, true, false, false, true>,
                    (true, true, true, false, false, false) => single_data_transfer::<true, true, true, false, false, false>,
                    (true, true, false, true, true, true) => single_data_transfer::<true, true, false, true, true, true>,
                    (true, true, false, true, true, false) => single_data_transfer::<true, true, false, true, true, false>,
                    (true, true, false, true, false, true) => single_data_transfer::<true, true, false, true, false, true>,
                    (true, true, false, true, false, false) => single_data_transfer::<true, true, false, true, false, false>,
                    (true, true, false, false, true, true) => single_data_transfer::<true, true, false, false, true, true>,
                    (true, true, false, false, true, false) => single_data_transfer::<true, true, false, false, true, false>,
                    (true, true, false, false, false, true) => single_data_transfer::<true, true, false, false, false, true>,
                    (true, true, false, false, false, false) => single_data_transfer::<true, true, false, false, false, false>,
                    (true, false, true, true, true, true) => single_data_transfer::<true, false, true, true, true, true>,
                    (true, false, true, true, true, false) => single_data_transfer::<true, false, true, true, true, false>,
                    (true, false, true, true, false, true) => single_data_transfer::<true, false, true, true, false, true>,
                    (true, false, true, true, false, false) => single_data_transfer::<true, false, true, true, false, false>,
                    (true, false, true, false, true, true) => single_data_transfer::<true, false, true, false, true, true>,
                    (true, false, true, false, true, false) => single_data_transfer::<true, false, true, false, true, false>,
                    (true, false, true, false, false, true) => single_data_transfer::<true, false, true, false, false, true>,
                    (true, false, true, false, false, false) => single_data_transfer::<true, false, true, false, false, false>,
                    (true, false, false, true, true, true) => single_data_transfer::<true, false, false, true, true, true>,
                    (true, false, false, true, true, false) => single_data_transfer::<true, false, false, true, true, false>,
                    (true, false, false, true, false, true) => single_data_transfer::<true, false, false, true, false, true>,
                    (true, false, false, true, false, false) => single_data_transfer::<true, false, false, true, false, false>,
                    (true, false, false, false, true, true) => single_data_transfer::<true, false, false, false, true, true>,
                    (true, false, false, false, true, false) => single_data_transfer::<true, false, false, false, true, false>,
                    (true, false, false, false, false, true) => single_data_transfer::<true, false, false, false, false, true>,
                    (true, false, false, false, false, false) => single_data_transfer::<true, false, false, false, false, false>,
                    (false, true, true, true, true, true) => single_data_transfer::<false, true, true, true, true, true>,
                    (false, true, true, true, true, false) => single_data_transfer::<false, true, true, true, true, false>,
                    (false, true, true, true, false, true) => single_data_transfer::<false, true, true, true, false, true>,
                    (false, true, true, true, false, false) => single_data_transfer::<false, true, true, true, false, false>,
                    (false, true, true, false, true, true) => single_data_transfer::<false, true, true, false, true, true>,
                    (false, true, true, false, true, false) => single_data_transfer::<false, true, true, false, true, false>,
                    (false, true, true, false, false, true) => single_data_transfer::<false, true, true, false, false, true>,
                    (false, true, true, false, false, false) => single_data_transfer::<false, true, true, false, false, false>,
                    (false, true, false, true, true, true) => single_data_transfer::<false, true, false, true, true, true>,
                    (false, true, false, true, true, false) => single_data_transfer::<false, true, false, true, true, false>,
                    (false, true, false, true, false, true) => single_data_transfer::<false, true, false, true, false, true>,
                    (false, true, false, true, false, false) => single_data_transfer::<false, true, false, true, false, false>,
                    (false, true, false, false, true, true) => single_data_transfer::<false, true, false, false, true, true>,
                    (false, true, false, false, true, false) => single_data_transfer::<false, true, false, false, true, false>,
                    (false, true, false, false, false, true) => single_data_transfer::<false, true, false, false, false, true>,
                    (false, true, false, false, false, false) => single_data_transfer::<false, true, false, false, false, false>,
                    (false, false, true, true, true, true) => single_data_transfer::<false, false, true, true, true, true>,
                    (false, false, true, true, true, false) => single_data_transfer::<false, false, true, true, true, false>,
                    (false, false, true, true, false, true) => single_data_transfer::<false, false, true, true, false, true>,
                    (false, false, true, true, false, false) => single_data_transfer::<false, false, true, true, false, false>,
                    (false, false, true, false, true, true) => single_data_transfer::<false, false, true, false, true, true>,
                    (false, false, true, false, true, false) => single_data_transfer::<false, false, true, false, true, false>,
                    (false, false, true, false, false, true) => single_data_transfer::<false, false, true, false, false, true>,
                    (false, false, true, false, false, false) => single_data_transfer::<false, false, true, false, false, false>,
                    (false, false, false, true, true, true) => single_data_transfer::<false, false, false, true, true, true>,
                    (false, false, false, true, true, false) => single_data_transfer::<false, false, false, true, true, false>,
                    (false, false, false, true, false, true) => single_data_transfer::<false, false, false, true, false, true>,
                    (false, false, false, true, false, false) => single_data_transfer::<false, false, false, true, false, false>,
                    (false, false, false, false, true, true) => single_data_transfer::<false, false, false, false, true, true>,
                    (false, false, false, false, true, false) => single_data_transfer::<false, false, false, false, true, false>,
                    (false, false, false, false, false, true) => single_data_transfer::<false, false, false, false, false, true>,
                    (false, false, false, false, false, false) => single_data_transfer::<false, false, false, false, false, false>,
                }
            } 
        }
        0b10 => {
            if (instruction & 0b1110_0000_0000) == 0b1010_0000_0000 {
                let link = (instruction >> 8) & 1 == 1;

                match link {
                    true =>  branch_and_link::<true>,
                    false =>  branch_and_link::<false>,
                }
            } else if (instruction & 0b1110_0000_0000) == 0b1000_0000_0000 {
                let pre_index = (instruction >> 8) & 1 == 1;
                let increment = (instruction >> 7) & 1 == 1;
                let s_bit = (instruction >> 6) & 1 == 1;
                let write_back = (instruction >> 5) & 1 == 1;
                let load = (instruction >> 4) & 1 == 1;

                match (pre_index, increment, s_bit, write_back, load) {
                    (true, true, true, true, true) => block_data_transfer::<true, true, true, true, true>,
                    (true, true, true, true, false) => block_data_transfer::<true, true, true, true, false>,
                    (true, true, true, false, true) => block_data_transfer::<true, true, true, false, true>,
                    (true, true, true, false, false) => block_data_transfer::<true, true, true, false, false>,
                    (true, true, false, true, true) => block_data_transfer::<true, true, false, true, true>,
                    (true, true, false, true, false) => block_data_transfer::<true, true, false, true, false>,
                    (true, true, false, false, true) => block_data_transfer::<true, true, false, false, true>,
                    (true, true, false, false, false) => block_data_transfer::<true, true, false, false, false>,
                    (true, false, true, true, true) => block_data_transfer::<true, false, true, true, true>,
                    (true, false, true, true, false) => block_data_transfer::<true, false, true, true, false>,
                    (true, false, true, false, true) => block_data_transfer::<true, false, true, false, true>,
                    (true, false, true, false, false) => block_data_transfer::<true, false, true, false, false>,
                    (true, false, false, true, true) => block_data_transfer::<true, false, false, true, true>,
                    (true, false, false, true, false) => block_data_transfer::<true, false, false, true, false>,
                    (true, false, false, false, true) => block_data_transfer::<true, false, false, false, true>,
                    (true, false, false, false, false) => block_data_transfer::<true, false, false, false, false>,
                    (false, true, true, true, true) => block_data_transfer::<false, true, true, true, true>,
                    (false, true, true, true, false) => block_data_transfer::<false, true, true, true, false>,
                    (false, true, true, false, true) => block_data_transfer::<false, true, true, false, true>,
                    (false, true, true, false, false) => block_data_transfer::<false, true, true, false, false>,
                    (false, true, false, true, true) => block_data_transfer::<false, true, false, true, true>,
                    (false, true, false, true, false) => block_data_transfer::<false, true, false, true, false>,
                    (false, true, false, false, true) => block_data_transfer::<false, true, false, false, true>,
                    (false, true, false, false, false) => block_data_transfer::<false, true, false, false, false>,
                    (false, false, true, true, true) => block_data_transfer::<false, false, true, true, true>,
                    (false, false, true, true, false) => block_data_transfer::<false, false, true, true, false>,
                    (false, false, true, false, true) => block_data_transfer::<false, false, true, false, true>,
                    (false, false, true, false, false) => block_data_transfer::<false, false, true, false, false>,
                    (false, false, false, true, true) => block_data_transfer::<false, false, false, true, true>,
                    (false, false, false, true, false) => block_data_transfer::<false, false, false, true, false>,
                    (false, false, false, false, true) => block_data_transfer::<false, false, false, false, true>,
                    (false, false, false, false, false) => block_data_transfer::<false, false, false, false, false>,
                }
            } 
            else {
                undefined_arm
            }
        }
        0b11 => undefined_arm,
        _ => panic!("invalid opcode"),
    }
}

const fn generate_arm_halfword_transfer<const IS_IMMEDIATE: bool>(instruction: usize) -> ArmHandler {
    use crate::arm::arm_handlers::halfword_and_signed_data_transfer;

    let pre_indexing = (instruction >> 8) & 1 == 1;
    let increment = (instruction >> 7) & 1 == 1;
    let write_back = (instruction >> 5) & 1 == 1;
    let load = (instruction >> 4) & 1 == 1;
    let s = (instruction >> 2) & 1 == 1;
    let h = (instruction >> 1) & 1 == 1;

    match (pre_indexing, increment, write_back, load, s, h) {
        (true, true, true, true, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, true, true, true, true>,
        (true, true, true, true, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, true, true, true, false>,
        (true, true, true, true, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, true, true, false, true>,
        (true, true, true, true, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, true, true, false, false>,
        (true, true, true, false, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, true, false, true, true>,
        (true, true, true, false, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, true, false, true, false>,
        (true, true, true, false, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, true, false, false, true>,
        (true, true, true, false, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, true, false, false, false>,
        (true, true, false, true, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, false, true, true, true>,
        (true, true, false, true, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, false, true, true, false>,
        (true, true, false, true, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, false, true, false, true>,
        (true, true, false, true, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, false, true, false, false>,
        (true, true, false, false, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, false, false, true, true>,
        (true, true, false, false, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, false, false, true, false>,
        (true, true, false, false, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, false, false, false, true>,
        (true, true, false, false, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, true, false, false, false, false>,
        (true, false, true, true, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, true, true, true, true>,
        (true, false, true, true, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, true, true, true, false>,
        (true, false, true, true, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, true, true, false, true>,
        (true, false, true, true, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, true, true, false, false>,
        (true, false, true, false, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, true, false, true, true>,
        (true, false, true, false, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, true, false, true, false>,
        (true, false, true, false, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, true, false, false, true>,
        (true, false, true, false, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, true, false, false, false>,
        (true, false, false, true, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, false, true, true, true>,
        (true, false, false, true, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, false, true, true, false>,
        (true, false, false, true, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, false, true, false, true>,
        (true, false, false, true, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, false, true, false, false>,
        (true, false, false, false, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, false, false, true, true>,
        (true, false, false, false, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, false, false, true, false>,
        (true, false, false, false, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, false, false, false, true>,
        (true, false, false, false, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, true, false, false, false, false, false>,
        (false, true, true, true, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, true, true, true, true>,
        (false, true, true, true, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, true, true, true, false>,
        (false, true, true, true, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, true, true, false, true>,
        (false, true, true, true, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, true, true, false, false>,
        (false, true, true, false, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, true, false, true, true>,
        (false, true, true, false, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, true, false, true, false>,
        (false, true, true, false, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, true, false, false, true>,
        (false, true, true, false, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, true, false, false, false>,
        (false, true, false, true, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, false, true, true, true>,
        (false, true, false, true, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, false, true, true, false>,
        (false, true, false, true, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, false, true, false, true>,
        (false, true, false, true, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, false, true, false, false>,
        (false, true, false, false, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, false, false, true, true>,
        (false, true, false, false, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, false, false, true, false>,
        (false, true, false, false, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, false, false, false, true>,
        (false, true, false, false, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, true, false, false, false, false>,
        (false, false, true, true, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, true, true, true, true>,
        (false, false, true, true, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, true, true, true, false>,
        (false, false, true, true, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, true, true, false, true>,
        (false, false, true, true, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, true, true, false, false>,
        (false, false, true, false, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, true, false, true, true>,
        (false, false, true, false, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, true, false, true, false>,
        (false, false, true, false, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, true, false, false, true>,
        (false, false, true, false, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, true, false, false, false>,
        (false, false, false, true, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, false, true, true, true>,
        (false, false, false, true, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, false, true, true, false>,
        (false, false, false, true, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, false, true, false, true>,
        (false, false, false, true, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, false, true, false, false>,
        (false, false, false, false, true, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, false, false, true, true>,
        (false, false, false, false, true, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, false, false, true, false>,
        (false, false, false, false, false, true) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, false, false, false, true>,
        (false, false, false, false, false, false) => halfword_and_signed_data_transfer::<IS_IMMEDIATE, false, false, false, false, false, false>,
    }
}
