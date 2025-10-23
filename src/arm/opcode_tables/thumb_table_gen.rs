use crate::arm::core::Arm7tdmi;
use crate::arm::opcode_tables::thumb_handlers::*;

pub(crate) type ThumbHandler = fn(&mut Arm7tdmi, u16);
pub(crate) const THUMB_TABLE_SIZE: usize = 0x400;

pub(crate) const fn generate_thumb_table() -> [ThumbHandler; THUMB_TABLE_SIZE] {
    use crate::arm::opcode_tables::thumb_handlers::*;

    let mut thumb_table: [ThumbHandler; THUMB_TABLE_SIZE] = [undefined_thumb; THUMB_TABLE_SIZE];

    let mut i = 0;

    while i < thumb_table.len() {
        thumb_table[i] = generate_thumb_instruction(i);
        i += 1;
    }

    thumb_table
}

const fn generate_thumb_instruction(instruction: usize) -> ThumbHandler {
    match instruction >> 8 {
        0b00 => {
            if (instruction & 0b11_1110_0000) == 0b00_0110_0000 {
                let is_immediate = (instruction >> 4) & 1 == 1;
                let is_subtract = (instruction >> 3) & 1 == 1;

                match (is_immediate, is_subtract) {
                    (true, true) => add_subtract::<true, true>,
                    (true, false) => add_subtract::<true, false>,
                    (false, true) => add_subtract::<false, true>,
                    (false, false) => add_subtract::<false, false>,
                }
            } else if (instruction & 0b11_1000_0000) == 0b00_0000_0000 {
                let shift_op = (instruction >> 5) & 0x3;
                match shift_op {
                    0 => move_shifted::<0>,
                    1 => move_shifted::<1>,
                    2 => move_shifted::<2>,
                    _ => panic!("Invalid shift op!"),
                }
            } else {
                undefined_thumb
            }
        }
        0b01 => undefined_thumb,
        0b10 => undefined_thumb,
        0b11 => undefined_thumb,
        _ => panic!("Invalid opcode!"),
    }
}
