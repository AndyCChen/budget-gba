use crate::arm::core::Arm7tdmi;
use crate::arm::opcode_tables::thumb_handlers::*;
use crate::bus::BusInterface;

pub type ThumbHandler<T> = fn(&mut Arm7tdmi<T>, &mut T, u16);
pub const THUMB_TABLE_SIZE: usize = 0x400;

pub const fn generate_thumb_table<T: BusInterface>() -> [ThumbHandler<T>; THUMB_TABLE_SIZE] {
    use crate::arm::opcode_tables::thumb_handlers::*;

    let mut thumb_table: [ThumbHandler<T>; THUMB_TABLE_SIZE] = [undefined_thumb; THUMB_TABLE_SIZE];

    let mut i = 0;
    while i < thumb_table.len() {
        thumb_table[i] = generate_thumb_instruction(i);
        i += 1;
    }

    thumb_table
}

const fn generate_thumb_instruction<T: BusInterface>(instruction: usize) -> ThumbHandler<T> {
    if (instruction & 0b11_1110_0000) == 0b00_0110_0000 {
        let is_immediate = (instruction >> 4) & 1 == 1;
        let is_subtract = (instruction >> 3) & 1 == 1;

        match (is_immediate, is_subtract) {
            (true, true) => add_subtract::<T, true, true>,
            (true, false) => add_subtract::<T, true, false>,
            (false, true) => add_subtract::<T, false, true>,
            (false, false) => add_subtract::<T, false, false>,
        }
    } else if (instruction & 0b11_1000_0000) == 0b00_0000_0000 {
        let shift_op = (instruction >> 5) & 0x3;

        match shift_op {
            0 => move_shifted::<T, 0>,
            1 => move_shifted::<T, 1>,
            2 => move_shifted::<T, 2>,
            _ => panic!("Invalid shift op!"),
        }
    } else if (instruction & 0b11_1000_0000) == 0b00_1000_0000 {
        let op = (instruction >> 5) & 0x3;

        match op {
            0 => mov_cmp_add_sub_immediate::<T, 0>,
            1 => mov_cmp_add_sub_immediate::<T, 1>,
            2 => mov_cmp_add_sub_immediate::<T, 2>,
            3 => mov_cmp_add_sub_immediate::<T, 3>,
            _ => panic!("Invalid op!"),
        }
    } else if (instruction & 0b11_1111_0000) == 0b01_0000_0000 {
        let op = instruction & 0xF;

        match op {
            0 => alu_operations::<T, 0>,
            1 => alu_operations::<T, 1>,
            2 => alu_operations::<T, 2>,
            3 => alu_operations::<T, 3>,
            4 => alu_operations::<T, 4>,
            5 => alu_operations::<T, 5>,
            6 => alu_operations::<T, 6>,
            7 => alu_operations::<T, 7>,
            8 => alu_operations::<T, 8>,
            9 => alu_operations::<T, 9>,
            10 => alu_operations::<T, 10>,
            11 => alu_operations::<T, 11>,
            12 => alu_operations::<T, 12>,
            13 => alu_operations::<T, 13>,
            14 => alu_operations::<T, 14>,
            15 => alu_operations::<T, 15>,
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
            (AddCmpMovBxOp::Add, true, true) => add_cmp_mov_hi::<T, 0, true, true>,
            (AddCmpMovBxOp::Add, true, false) => add_cmp_mov_hi::<T, 0, true, false>,
            (AddCmpMovBxOp::Add, false, true) => add_cmp_mov_hi::<T, 0, false, true>,
            (AddCmpMovBxOp::Add, false, false) => add_cmp_mov_hi::<T, 0, false, false>,

            (AddCmpMovBxOp::Cmp, true, true) => add_cmp_mov_hi::<T, 1, true, true>,
            (AddCmpMovBxOp::Cmp, true, false) => add_cmp_mov_hi::<T, 1, true, false>,
            (AddCmpMovBxOp::Cmp, false, true) => add_cmp_mov_hi::<T, 1, false, true>,
            (AddCmpMovBxOp::Cmp, false, false) => add_cmp_mov_hi::<T, 1, false, false>,

            (AddCmpMovBxOp::Mov, true, true) => add_cmp_mov_hi::<T, 2, true, true>,
            (AddCmpMovBxOp::Mov, true, false) => add_cmp_mov_hi::<T, 2, true, false>,
            (AddCmpMovBxOp::Mov, false, true) => add_cmp_mov_hi::<T, 2, false, true>,
            (AddCmpMovBxOp::Mov, false, false) => add_cmp_mov_hi::<T, 2, false, false>,

            (AddCmpMovBxOp::Bx, true, true) => add_cmp_mov_hi::<T, 3, true, true>,
            (AddCmpMovBxOp::Bx, true, false) => add_cmp_mov_hi::<T, 3, true, false>,
            (AddCmpMovBxOp::Bx, false, true) => add_cmp_mov_hi::<T, 3, false, true>,
            (AddCmpMovBxOp::Bx, false, false) => add_cmp_mov_hi::<T, 3, false, false>,
        }
    } else if (instruction & 0b11_1110_0000) == 0b01_0010_0000 {
        pc_relative_load
    } else if (instruction & 0b11_1100_1000) == 0b01_0100_0000 {
        let is_load = (instruction >> 5) & 1 == 1;
        let is_byte_sized = (instruction >> 4) & 1 == 1;

        match (is_load, is_byte_sized) {
            (true, true) => load_store_register_offset::<T, true, true>,
            (true, false) => load_store_register_offset::<T, true, false>,
            (false, true) => load_store_register_offset::<T, false, true>,
            (false, false) => load_store_register_offset::<T, false, false>,
        }
    } else if (instruction & 0b11_1100_1000) == 0b01_0100_1000 {
        let op = (instruction >> 4) & 0x3;

        match op {
            0 => load_store_sign_extended::<T, 0>,
            1 => load_store_sign_extended::<T, 1>,
            2 => load_store_sign_extended::<T, 2>,
            3 => load_store_sign_extended::<T, 3>,
            _ => panic!("Invalid op!"),
        }
    } else if (instruction & 0b11_1000_0000) == 0b01_1000_0000 {
        let transfer_byte = (instruction >> 6) & 1 == 1;
        let is_load = (instruction >> 5) & 1 == 1;

        match (transfer_byte, is_load) {
            (true, true) => load_store_immediate_offset::<T, true, true>,
            (true, false) => load_store_immediate_offset::<T, true, false>,
            (false, true) => load_store_immediate_offset::<T, false, true>,
            (false, false) => load_store_immediate_offset::<T, false, false>,
        }
    } else if (instruction & 0b11_1100_0000) == 0b10_0000_0000 {
        let is_load = (instruction >> 5) & 1 == 1;

        match is_load {
            true => load_store_halfword_immediate_offset::<T, true>,
            false => load_store_halfword_immediate_offset::<T, false>,
        }
    } else if (instruction & 0b11_1100_0000) == 0b10_0100_0000 {
        let is_load = (instruction >> 5) & 1 == 1;

        match is_load {
            true => sp_load_store_relative_offset::<T, true>,
            false => sp_load_store_relative_offset::<T, false>,
        }
    } else if (instruction & 0b11_1100_0000) == 0b10_1000_0000 {
        let is_stack_pointer = (instruction >> 5) & 1 == 1;

        match is_stack_pointer {
            true => pc_sp_load_address::<T, true>,
            false => pc_sp_load_address::<T, false>,
        }
    } else if (instruction & 0b11_1111_1100) == 0b10_1100_0000 {
        let is_negative = (instruction >> 1) & 1 == 1;

        match is_negative {
            true => add_sub_sp::<T, true>,
            false => add_sub_sp::<T, false>,
        }
    } else if (instruction & 0b11_1101_1000) == 0b10_1101_0000 {
        let is_load = (instruction >> 5) & 1 == 1;
        let push_pop_lr_pc = (instruction >> 2) & 1 == 1;

        match (is_load, push_pop_lr_pc) {
            (true, true) => push_pop_register::<T, true, true>,
            (true, false) => push_pop_register::<T, true, false>,
            (false, true) => push_pop_register::<T, false, true>,
            (false, false) => push_pop_register::<T, false, false>,
        }
    } else if (instruction & 0b11_1100_0000) == 0b11_0000_0000 {
        let is_load = (instruction >> 5) & 1 == 1;

        match is_load {
            true => multiple_load_store::<T, true>,
            false => multiple_load_store::<T, false>,
        }
    } else if (instruction & 0b11_1100_0000) == 0b11_0100_0000 {
        let cond = (instruction >> 2) & 0xF;

        match cond {
            0 => conditional_branch::<T, 0>,
            1 => conditional_branch::<T, 1>,
            2 => conditional_branch::<T, 2>,
            3 => conditional_branch::<T, 3>,
            4 => conditional_branch::<T, 4>,
            5 => conditional_branch::<T, 5>,
            6 => conditional_branch::<T, 6>,
            7 => conditional_branch::<T, 7>,
            8 => conditional_branch::<T, 8>,
            9 => conditional_branch::<T, 9>,
            10 => conditional_branch::<T, 10>,
            11 => conditional_branch::<T, 11>,
            12 => conditional_branch::<T, 12>,
            13 => conditional_branch::<T, 13>,
            14 => conditional_branch::<T, 14>,
            15 => software_interrupt,
            _ => panic!("Invalid OP"),
        }
    } else if (instruction & 0b11_1110_0000) == 0b11_1000_0000 {
        unconditional_branch
    } else if (instruction & 0b11_1100_0000) == 0b11_1100_0000 {
        let h_bit = (instruction >> 5) & 1 == 1;

        match h_bit {
            true => long_branch_with_link::<T, true>,
            false => long_branch_with_link::<T, false>,
        }
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
