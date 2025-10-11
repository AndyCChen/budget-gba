use crate::arm::core::Arm7tdmi;

const ARM_TABLE_LENGTH: usize = 0x1000;

pub static ARM_TABLE: [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] = generate_arm_table();

const fn generate_arm_table() -> [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] {
    use crate::arm::arm_handlers::*;

    let mut arm_table: [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] =
        [undefined_arm; ARM_TABLE_LENGTH];

    let mut i = 0;

    while i < ARM_TABLE_LENGTH {
        arm_table[i] = generate_arm_instruction(i);
        i += 1;
    }

    arm_table
}

const fn generate_arm_instruction(instruction: usize) -> fn(&mut Arm7tdmi, u32) {
    use crate::arm::arm_handlers::*;
    use crate::{_data_processing_inner, data_processing};

    match instruction >> 10 {
        0b00 => {
            if instruction == 0b0001_0010_0001 {
                branch_and_exchange
            } else if (instruction & 0b0001_1011_0000) == 0b0001_0000_0000 {
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

                if set_condition {
                    data_processing!(true, data_opcode, true, shift_field)
                } else {
                    data_processing!(true, data_opcode, false, shift_field)
                }
            }
            // data proc non-immediate mode
            else if (instruction & 0b0010_0000_0000) == 0 && (instruction & 0b1001) != 0b1001 {
                let data_opcode: u8 = ((instruction >> 5) & 0xF) as u8;
                let shift_field: u8 = (instruction & 0xF) as u8;
                let set_condition = (instruction & 0b0000_0001_0000) != 0;

                if set_condition {
                    data_processing!(false, data_opcode, true, shift_field)
                } else {
                    data_processing!(false, data_opcode, false, shift_field)
                }
            } else if (instruction & 0b1111_1100_1001) == 0b0000_0000_1001 {
                let accumulate = (instruction >> 5) & 1 != 0;
                let set_condition = (instruction >> 4) & 1 != 0;

                match (accumulate, set_condition) {
                    (true, true) => multiply::<true, true>,
                    (true, false) => multiply::<true, false>,
                    (false, true) => multiply::<false, true>,
                    (false, false) => multiply::<false, false>,
                }
            } else if (instruction & 0b1111_1000_1001) == 0b0000_1000_1001 {
                let signed = (instruction >> 6) & 1 != 0;
                let accumulate = (instruction >> 5) & 1 != 0;
                let set_condition = (instruction >> 4) & 1 != 0;

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
            } else {
                undefined_arm
            }
        }
        0b01 => undefined_arm,
        0b10 => {
            if (instruction & 0xF00) == 0b1010_0000_0000 {
                branch_and_link::<false>
            } else if (instruction & 0xF00) == 0b1011_0000_0000 {
                branch_and_link::<true>
            } else {
                undefined_arm
            }
        }
        0b11 => undefined_arm,
        _ => panic!("invalid opcode"),
    }
}
