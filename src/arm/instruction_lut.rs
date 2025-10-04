use crate::arm::core::Arm7tdmi;

const ARM_TABLE_LENGTH: usize = 0x1000;

pub static ARM_TABLE: [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] = generate_arm_table();

const fn generate_arm_table() -> [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] {
    use crate::arm::arm_handlers::*;

    macro_rules! data_processing {
        ($imm:expr, $data_opcode:expr, $set_cond:expr) => {
            match $data_opcode {
                0 => data_processing::<$imm, { data_op::AND }, $set_cond>,
                1 => data_processing::<$imm, { data_op::EOR }, $set_cond>,
                2 => data_processing::<$imm, { data_op::SUB }, $set_cond>,
                3 => data_processing::<$imm, { data_op::RSB }, $set_cond>,
                4 => data_processing::<$imm, { data_op::ADD }, $set_cond>,
                5 => data_processing::<$imm, { data_op::ADC }, $set_cond>,
                6 => data_processing::<$imm, { data_op::SBC }, $set_cond>,
                7 => data_processing::<$imm, { data_op::RSC }, $set_cond>,
                8 => data_processing::<$imm, { data_op::TST }, $set_cond>,
                9 => data_processing::<$imm, { data_op::TEQ }, $set_cond>,
                10 => data_processing::<$imm, { data_op::CMP }, $set_cond>,
                11 => data_processing::<$imm, { data_op::CMN }, $set_cond>,
                12 => data_processing::<$imm, { data_op::ORR }, $set_cond>,
                13 => data_processing::<$imm, { data_op::MOV }, $set_cond>,
                14 => data_processing::<$imm, { data_op::BIC }, $set_cond>,
                15 => data_processing::<$imm, { data_op::MVN }, $set_cond>,

                _ => panic!("Invalide data op!"),
            }
        };
    }

    let mut arm_table: [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] =
        [undefined_arm; ARM_TABLE_LENGTH];

    let mut i = 0;

    while i < arm_table.len() {
        arm_table[i] = match i >> 10 {
            0b00 => {
                if i == 0b0001_0010_0001 {
                    branch_and_exchange
                } else if (i & 0b0010_0000_0000) != 0 {
                    let data_opcode = (i >> 5) & 0xF;

                    if (i & 0b0000_0001_0000) == 0 {
                        data_processing!(true, data_opcode, false)
                    } else {
                        data_processing!(true, data_opcode, true)
                    }
                } else if (i & 0b0010_0000_0000) == 0 && (i & 0b1001) != 0b1001 {
                    let data_opcode = (i >> 5) & 0xF;

                    if (i & 0b0010_0001_0000) == 0 {
                        data_processing!(false, data_opcode, false)
                    } else {
                        data_processing!(false, data_opcode, true)
                    }
                } else {
                    undefined_arm
                }
            }
            0b01 => undefined_arm,
            0b10 => {
                if (i & 0xF00) == 0b1010_0000_0000 {
                    branch_and_link::<false>
                } else if (i & 0xF00) == 0b1011_0000_0000 {
                    branch_and_link::<true>
                } else {
                    undefined_arm
                }
            }
            0b11 => undefined_arm,
            _ => panic!("invalid opcode"),
        };

        i += 1;
    }

    arm_table
}
