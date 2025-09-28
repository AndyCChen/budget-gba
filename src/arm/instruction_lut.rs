use crate::arm::core::Arm7tdmi;

const ARM_TABLE_LENGTH: usize = 0x1000;

pub const ARM_TABLE: [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] = generate_arm_table();

const fn generate_arm_table() -> [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] {
    use crate::arm::arm_decoder::*;

    let mut arm_table: [fn(&mut Arm7tdmi, u32); ARM_TABLE_LENGTH] =
        [undefined_arm; ARM_TABLE_LENGTH];

    let mut i = 0;

    while i < arm_table.len() {
        arm_table[i] = match i >> 10 {
            0b00 => {
                if i == 0b000100100001 {
                    branch_and_exchange
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
                }
                else {
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
