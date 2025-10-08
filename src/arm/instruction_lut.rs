use crate::arm::core::Arm7tdmi;

const ARM_TABLE_LENGTH: usize = 0x1000;

pub static ARM_TABLE: [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] = generate_arm_table();

const fn generate_arm_table() -> [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] {
    use crate::arm::arm_handlers::*;

    let mut arm_table: [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] =
        [undefined_arm; ARM_TABLE_LENGTH];

    let mut i = 0;

    while i < arm_table.len() {
        arm_table[i] = generate_arm_instruction(i);
        i += 1;
    }

    arm_table
}

const fn generate_arm_instruction(instruction: usize) -> fn(&mut Arm7tdmi, u32) {
    use crate::arm::arm_handlers::*;
    use crate::{data_processing, _data_processing_inner};

    match instruction >> 10 {
        0b00 => {
            if instruction == 0b0001_0010_0001 {
                branch_and_exchange
            } else if (instruction & 0b0010_0000_0000) != 0 {
                let data_opcode: u8 = ((instruction >> 5) & 0xF) as u8;
                let shift_field: u8 = (instruction & 0xF) as u8;

                if (instruction & 0b0000_0001_0000) == 0 {
                    data_processing!(true, data_opcode, false, shift_field)
                } else {
                    data_processing!(true, data_opcode, true, shift_field)
                }
            } else if (instruction & 0b0010_0000_0000) == 0 && (instruction & 0b1001) != 0b1001 {
                let data_opcode: u8 = ((instruction >> 5) & 0xF) as u8;
                let shift_field: u8 = (instruction & 0xF) as u8;

                if (instruction & 0b0010_0001_0000) == 0 {
                    data_processing!(false, data_opcode, false, shift_field)
                } else {
                    data_processing!(false, data_opcode, true, shift_field)
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
