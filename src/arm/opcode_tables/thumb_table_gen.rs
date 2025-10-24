use crate::arm::core::Arm7tdmi;
use crate::arm::opcode_tables::thumb_handlers::*;

pub type ThumbHandler = fn(&mut Arm7tdmi, u16);
pub const THUMB_TABLE_SIZE: usize = 0x400;

pub const fn generate_thumb_table() -> [ThumbHandler; THUMB_TABLE_SIZE] {
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
    } else if (instruction & 0b11_1000_0000) == 0b00_1000_0000 {
        let op = (instruction >> 5) & 0x3;

        match op {
            0 => mov_cmp_add_sub_immediate::<0>,
            1 => mov_cmp_add_sub_immediate::<1>,
            2 => mov_cmp_add_sub_immediate::<2>,
            3 => mov_cmp_add_sub_immediate::<3>,
            _ => panic!("Invalid op!"),
        }
    } else if (instruction & 0b11_1111_0000) == 0b01_0000_0000 {
        let op = instruction & 0xF;

        match op {
            0 => alu_operations::<0>,
            1 => alu_operations::<1>,
            2 => alu_operations::<2>,
            3 => alu_operations::<3>,
            4 => alu_operations::<4>,
            5 => alu_operations::<5>,
            6 => alu_operations::<6>,
            7 => alu_operations::<7>,
            8 => alu_operations::<8>,
            9 => alu_operations::<9>,
            10 => alu_operations::<10>,
            11 => alu_operations::<11>,
            12 => alu_operations::<12>,
            13 => alu_operations::<13>,
            14 => alu_operations::<14>,
            15 => alu_operations::<15>,
            _ => panic!("Invalid op!"),
        }
    } else if (instruction & 0b11_1111_0000) == 0b01_0001_0000 {
        let h1 = (instruction >> 1) & 1 == 1;
        let h2 = instruction & 1 == 1;
        let op_type = match (instruction >> 2) & 0x3 {
            0 => AddCmpMovBxOp::Add,
            1 => AddCmpMovBxOp::Cmp,
            2 => AddCmpMovBxOp::Mov,
            3 => AddCmpMovBxOp::Bx,
            _ => panic!("Invalid op!"),
        };

        match (op_type, h1, h2) {
            (AddCmpMovBxOp::Add, true, true) => add_cmp_mov_hi::<0, true, true>,
            (AddCmpMovBxOp::Add, true, false) => add_cmp_mov_hi::<0, true, false>,
            (AddCmpMovBxOp::Add, false, true) => add_cmp_mov_hi::<0, false, true>,
            (AddCmpMovBxOp::Add, false, false) => add_cmp_mov_hi::<0, false, false>,

            (AddCmpMovBxOp::Cmp, true, true) => add_cmp_mov_hi::<1, true, true>,
            (AddCmpMovBxOp::Cmp, true, false) => add_cmp_mov_hi::<1, true, false>,
            (AddCmpMovBxOp::Cmp, false, true) => add_cmp_mov_hi::<1, false, true>,
            (AddCmpMovBxOp::Cmp, false, false) => add_cmp_mov_hi::<1, false, false>,

            (AddCmpMovBxOp::Mov, true, true) => add_cmp_mov_hi::<2, true, true>,
            (AddCmpMovBxOp::Mov, true, false) => add_cmp_mov_hi::<2, true, false>,
            (AddCmpMovBxOp::Mov, false, true) => add_cmp_mov_hi::<2, false, true>,
            (AddCmpMovBxOp::Mov, false, false) => add_cmp_mov_hi::<2, false, false>,

            (AddCmpMovBxOp::Bx, true, true) => add_cmp_mov_hi::<3, true, true>,
            (AddCmpMovBxOp::Bx, true, false) => add_cmp_mov_hi::<3, true, false>,
            (AddCmpMovBxOp::Bx, false, true) => add_cmp_mov_hi::<3, false, true>,
            (AddCmpMovBxOp::Bx, false, false) => add_cmp_mov_hi::<3, false, false>,
        }
    } else if (instruction & 0b11_1110_0000) == 0b01_0010_0000 {
        pc_relative_load
    } else {
        undefined_thumb
    }
}

enum AddCmpMovBxOp {
    Add = 0,
    Cmp = 1,
    Mov = 2,
    Bx = 3,
}
