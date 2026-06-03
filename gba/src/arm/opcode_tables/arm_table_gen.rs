use super::common::arm_data_op;
use crate::arm::core::Arm7tdmi;
use crate::bus::BusInterface;

pub type ArmHandler<T> = fn(&mut Arm7tdmi<T>, &mut T, u32);
pub const ARM_TABLE_SIZE: usize = 0x1000;

pub const fn generate_arm_table<T: BusInterface>() -> [ArmHandler<T>; ARM_TABLE_SIZE] {
    use crate::arm::opcode_tables::arm_handlers::undefined_arm;

    let mut arm_table: [ArmHandler<T>; ARM_TABLE_SIZE] = [undefined_arm; ARM_TABLE_SIZE];

    let mut i = 0;
    while i < arm_table.len() {
        arm_table[i] = generate_arm_instruction(i);
        i += 1;
    }

    arm_table
}

/// poor man's macro to help generate data processing instructions for the arm lookup table at compile time
#[rustfmt::skip]
macro_rules! data_processing {
    ($imm:expr, $data_opcode:expr, $set_cond:expr, $shift:expr) => {
        match $data_opcode {
            arm_data_op::AND => _data_processing_inner!($imm, { arm_data_op::AND }, $set_cond, $shift),
            arm_data_op::EOR => _data_processing_inner!($imm, { arm_data_op::EOR }, $set_cond, $shift),
            arm_data_op::SUB => _data_processing_inner!($imm, { arm_data_op::SUB }, $set_cond, $shift),
            arm_data_op::RSB => _data_processing_inner!($imm, { arm_data_op::RSB }, $set_cond, $shift),
            arm_data_op::ADD => _data_processing_inner!($imm, { arm_data_op::ADD }, $set_cond, $shift),
            arm_data_op::ADC => _data_processing_inner!($imm, { arm_data_op::ADC }, $set_cond, $shift),
            arm_data_op::SBC => _data_processing_inner!($imm, { arm_data_op::SBC }, $set_cond, $shift),
            arm_data_op::RSC => _data_processing_inner!($imm, { arm_data_op::RSC }, $set_cond, $shift),
            arm_data_op::TST => _data_processing_inner!($imm, { arm_data_op::TST }, $set_cond, $shift),
            arm_data_op::TEQ => _data_processing_inner!($imm, { arm_data_op::TEQ }, $set_cond, $shift),
            arm_data_op::CMP => _data_processing_inner!($imm, { arm_data_op::CMP }, $set_cond, $shift),
            arm_data_op::CMN => _data_processing_inner!($imm, { arm_data_op::CMN }, $set_cond, $shift),
            arm_data_op::ORR => _data_processing_inner!($imm, { arm_data_op::ORR }, $set_cond, $shift),
            arm_data_op::MOV => _data_processing_inner!($imm, { arm_data_op::MOV }, $set_cond, $shift),
            arm_data_op::BIC => _data_processing_inner!($imm, { arm_data_op::BIC }, $set_cond, $shift),
            arm_data_op::MVN => _data_processing_inner!($imm, { arm_data_op::MVN }, $set_cond, $shift),
            _ => panic!("Invalid data op!"),
        }
    };
}

#[rustfmt::skip]
macro_rules! _data_processing_inner {
    ($imm:expr, $data_opcode:expr, $set_cond:expr, $shift:expr) => {
        match $shift {
            0 => data_processing::<T, $imm, $data_opcode, $set_cond, 0>,
            1 => data_processing::<T, $imm, $data_opcode, $set_cond, 1>,
            2 => data_processing::<T, $imm, $data_opcode, $set_cond, 2>,
            3 => data_processing::<T, $imm, $data_opcode, $set_cond, 3>,
            4 => data_processing::<T, $imm, $data_opcode, $set_cond, 4>,
            5 => data_processing::<T, $imm, $data_opcode, $set_cond, 5>,
            6 => data_processing::<T, $imm, $data_opcode, $set_cond, 6>,
            7 => data_processing::<T, $imm, $data_opcode, $set_cond, 7>,
            8 => data_processing::<T, $imm, $data_opcode, $set_cond, 8>,
            9 => data_processing::<T, $imm, $data_opcode, $set_cond, 9>,
            10 => data_processing::<T, $imm, $data_opcode, $set_cond, 10>,
            11 => data_processing::<T, $imm, $data_opcode, $set_cond, 11>,
            12 => data_processing::<T, $imm, $data_opcode, $set_cond, 12>,
            13 => data_processing::<T, $imm, $data_opcode, $set_cond, 13>,
            14 => data_processing::<T, $imm, $data_opcode, $set_cond, 14>,
            15 => data_processing::<T, $imm, $data_opcode, $set_cond, 15>,
            _ => panic!("shift field must be in range 0-15!"),
        }
    };
}

#[rustfmt::skip]
const fn generate_arm_instruction<T: BusInterface>(instruction: usize) -> ArmHandler<T>{
    use crate::arm::opcode_tables::arm_handlers::*;

    if instruction == 0b0001_0010_0001 {
        branch_and_exchange
    }
    else if (instruction & 0b1111_1011_1111) == 0b0001_0000_1001 {
        let byte_quantity = (instruction >> 6) & 1 == 1;
        match byte_quantity {
            true => data_swap::<T, true>,
            false => data_swap::<T, false>,
        }
    }  
    else if (instruction & 0b1110_0000_1001) == 0b0000_0000_1001 && (instruction & 0b0110) != 0 {
        let is_immediate = (instruction >> 6) & 1 == 1;

        match is_immediate {
            true => generate_arm_halfword_transfer::<T, true>(instruction),
            false => generate_arm_halfword_transfer::<T, false>(instruction)
        }
    }  
    else if (instruction & 0b1111_1011_0000) == 0b0001_0000_0000 {
        let is_source_spsr = (instruction & 0b0000_0100_0000) != 0;

        match is_source_spsr {
            true => read_status_mrs::<T, true>,
            false => read_status_mrs::<T, false>,
        }
    } else if (instruction & 0b1101_1011_0000) == 0b0001_0010_0000 {
        let is_immediate = (instruction & 0b0010_0000_0000) != 0;
        let is_source_spsr = (instruction & 0b0000_0100_0000) != 0;

        match (is_immediate, is_source_spsr) {
            (true, true) => write_status_msr::<T, true, true>,
            (true, false) => write_status_msr::<T, true, false>,
            (false, true) => write_status_msr::<T, false, true>,
            (false, false) => write_status_msr::<T, false, false>,
        }
    }
    // data proc immediate mode
    else if (instruction & 0b1110_0000_0000) == 0b0010_0000_0000 {
        let data_opcode: u8 = ((instruction >> 5) & 0xF) as u8;
        let shift_field: u8 = (instruction & 0xF) as u8;
        let set_condition = (instruction & 0b0000_0001_0000) != 0;

        match set_condition {
            true => data_processing!(true, data_opcode, true, shift_field),
            false => data_processing!(true, data_opcode, false, shift_field),
        }
    }
    // data proc non-immediate mode
    else if (instruction & 0b1110_0000_0000) == 0 && (instruction & 0b1001) != 0b1001 {
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
            (true, true) => multiply::<T, true, true>,
            (true, false) => multiply::<T, true, false>,
            (false, true) => multiply::<T, false, true>,
            (false, false) => multiply::<T, false, false>,
        }
    } else if (instruction & 0b1111_1000_1001) == 0b0000_1000_1001 {
        let signed = (instruction >> 6) & 1 == 1;
        let accumulate = (instruction >> 5) & 1 == 1;
        let set_condition = (instruction >> 4) & 1 == 1;

        match (signed, accumulate, set_condition) {
            (true, true, true) => multiply_long::<T, true, true, true>,
            (true, true, false) => multiply_long::<T, true, true, false>,
            (true, false, true) => multiply_long::<T, true, false, true>,
            (true, false, false) => multiply_long::<T, true, false, false>,
            (false, true, true) => multiply_long::<T, false, true, true>,
            (false, true, false) => multiply_long::<T, false, true, false>,
            (false, false, true) => multiply_long::<T, false, false, true>,
            (false, false, false) => multiply_long::<T, false, false, false>,
        }
    } else if (instruction & 0b1110_0000_0001) == 0b0110_0000_0001 {
        undefined_arm
    } else if (instruction & 0b1100_0000_0000) == 0b0100_0000_0000 {
        // 01IP_UBWL_****

        let is_immediate = (instruction >> 9) & 1 == 0;
        let pre_indexing = (instruction >> 8) & 1 == 1;
        let increment = (instruction >> 7) & 1 == 1;
        let byte_quantity = (instruction >> 6) & 1 == 1;
        let write_back = (instruction >> 5) & 1 == 1;
        let load = (instruction >> 4) & 1 == 1;

        match (is_immediate, pre_indexing, increment, byte_quantity, write_back, load) {
            (true, true, true, true, true, true) => single_data_transfer::<T, true, true, true, true, true, true>,
            (true, true, true, true, true, false) => single_data_transfer::<T, true, true, true, true, true, false>,
            (true, true, true, true, false, true) => single_data_transfer::<T, true, true, true, true, false, true>,
            (true, true, true, true, false, false) => single_data_transfer::<T, true, true, true, true, false, false>,
            (true, true, true, false, true, true) => single_data_transfer::<T, true, true, true, false, true, true>,
            (true, true, true, false, true, false) => single_data_transfer::<T, true, true, true, false, true, false>,
            (true, true, true, false, false, true) => single_data_transfer::<T, true, true, true, false, false, true>,
            (true, true, true, false, false, false) => single_data_transfer::<T, true, true, true, false, false, false>,
            (true, true, false, true, true, true) => single_data_transfer::<T, true, true, false, true, true, true>,
            (true, true, false, true, true, false) => single_data_transfer::<T, true, true, false, true, true, false>,
            (true, true, false, true, false, true) => single_data_transfer::<T, true, true, false, true, false, true>,
            (true, true, false, true, false, false) => single_data_transfer::<T, true, true, false, true, false, false>,
            (true, true, false, false, true, true) => single_data_transfer::<T, true, true, false, false, true, true>,
            (true, true, false, false, true, false) => single_data_transfer::<T, true, true, false, false, true, false>,
            (true, true, false, false, false, true) => single_data_transfer::<T, true, true, false, false, false, true>,
            (true, true, false, false, false, false) => single_data_transfer::<T, true, true, false, false, false, false>,
            (true, false, true, true, true, true) => single_data_transfer::<T, true, false, true, true, true, true>,
            (true, false, true, true, true, false) => single_data_transfer::<T, true, false, true, true, true, false>,
            (true, false, true, true, false, true) => single_data_transfer::<T, true, false, true, true, false, true>,
            (true, false, true, true, false, false) => single_data_transfer::<T, true, false, true, true, false, false>,
            (true, false, true, false, true, true) => single_data_transfer::<T, true, false, true, false, true, true>,
            (true, false, true, false, true, false) => single_data_transfer::<T, true, false, true, false, true, false>,
            (true, false, true, false, false, true) => single_data_transfer::<T, true, false, true, false, false, true>,
            (true, false, true, false, false, false) => single_data_transfer::<T, true, false, true, false, false, false>,
            (true, false, false, true, true, true) => single_data_transfer::<T, true, false, false, true, true, true>,
            (true, false, false, true, true, false) => single_data_transfer::<T, true, false, false, true, true, false>,
            (true, false, false, true, false, true) => single_data_transfer::<T, true, false, false, true, false, true>,
            (true, false, false, true, false, false) => single_data_transfer::<T, true, false, false, true, false, false>,
            (true, false, false, false, true, true) => single_data_transfer::<T, true, false, false, false, true, true>,
            (true, false, false, false, true, false) => single_data_transfer::<T, true, false, false, false, true, false>,
            (true, false, false, false, false, true) => single_data_transfer::<T, true, false, false, false, false, true>,
            (true, false, false, false, false, false) => single_data_transfer::<T, true, false, false, false, false, false>,
            (false, true, true, true, true, true) => single_data_transfer::<T, false, true, true, true, true, true>,
            (false, true, true, true, true, false) => single_data_transfer::<T, false, true, true, true, true, false>,
            (false, true, true, true, false, true) => single_data_transfer::<T, false, true, true, true, false, true>,
            (false, true, true, true, false, false) => single_data_transfer::<T, false, true, true, true, false, false>,
            (false, true, true, false, true, true) => single_data_transfer::<T, false, true, true, false, true, true>,
            (false, true, true, false, true, false) => single_data_transfer::<T, false, true, true, false, true, false>,
            (false, true, true, false, false, true) => single_data_transfer::<T, false, true, true, false, false, true>,
            (false, true, true, false, false, false) => single_data_transfer::<T, false, true, true, false, false, false>,
            (false, true, false, true, true, true) => single_data_transfer::<T, false, true, false, true, true, true>,
            (false, true, false, true, true, false) => single_data_transfer::<T, false, true, false, true, true, false>,
            (false, true, false, true, false, true) => single_data_transfer::<T, false, true, false, true, false, true>,
            (false, true, false, true, false, false) => single_data_transfer::<T, false, true, false, true, false, false>,
            (false, true, false, false, true, true) => single_data_transfer::<T, false, true, false, false, true, true>,
            (false, true, false, false, true, false) => single_data_transfer::<T, false, true, false, false, true, false>,
            (false, true, false, false, false, true) => single_data_transfer::<T, false, true, false, false, false, true>,
            (false, true, false, false, false, false) => single_data_transfer::<T, false, true, false, false, false, false>,
            (false, false, true, true, true, true) => single_data_transfer::<T, false, false, true, true, true, true>,
            (false, false, true, true, true, false) => single_data_transfer::<T, false, false, true, true, true, false>,
            (false, false, true, true, false, true) => single_data_transfer::<T, false, false, true, true, false, true>,
            (false, false, true, true, false, false) => single_data_transfer::<T, false, false, true, true, false, false>,
            (false, false, true, false, true, true) => single_data_transfer::<T, false, false, true, false, true, true>,
            (false, false, true, false, true, false) => single_data_transfer::<T, false, false, true, false, true, false>,
            (false, false, true, false, false, true) => single_data_transfer::<T, false, false, true, false, false, true>,
            (false, false, true, false, false, false) => single_data_transfer::<T, false, false, true, false, false, false>,
            (false, false, false, true, true, true) => single_data_transfer::<T, false, false, false, true, true, true>,
            (false, false, false, true, true, false) => single_data_transfer::<T, false, false, false, true, true, false>,
            (false, false, false, true, false, true) => single_data_transfer::<T, false, false, false, true, false, true>,
            (false, false, false, true, false, false) => single_data_transfer::<T, false, false, false, true, false, false>,
            (false, false, false, false, true, true) => single_data_transfer::<T, false, false, false, false, true, true>,
            (false, false, false, false, true, false) => single_data_transfer::<T, false, false, false, false, true, false>,
            (false, false, false, false, false, true) => single_data_transfer::<T, false, false, false, false, false, true>,
            (false, false, false, false, false, false) => single_data_transfer::<T, false, false, false, false, false, false>,
        }
    }  else if (instruction & 0b1110_0000_0000) == 0b1010_0000_0000 {
        let link = (instruction >> 8) & 1 == 1;

        match link {
            true =>  branch_and_link::<T, true>,
            false =>  branch_and_link::<T, false>,
        }
    } else if (instruction & 0b1110_0000_0000) == 0b1000_0000_0000 {
        let pre_index = (instruction >> 8) & 1 == 1;
        let increment = (instruction >> 7) & 1 == 1;
        let s_bit = (instruction >> 6) & 1 == 1;
        let write_back = (instruction >> 5) & 1 == 1;
        let load = (instruction >> 4) & 1 == 1;

        match (pre_index, increment, s_bit, write_back, load) {
            (true, true, true, true, true) => block_data_transfer::<T, true, true, true, true, true>,
            (true, true, true, true, false) => block_data_transfer::<T, true, true, true, true, false>,
            (true, true, true, false, true) => block_data_transfer::<T, true, true, true, false, true>,
            (true, true, true, false, false) => block_data_transfer::<T, true, true, true, false, false>,
            (true, true, false, true, true) => block_data_transfer::<T, true, true, false, true, true>,
            (true, true, false, true, false) => block_data_transfer::<T, true, true, false, true, false>,
            (true, true, false, false, true) => block_data_transfer::<T, true, true, false, false, true>,
            (true, true, false, false, false) => block_data_transfer::<T, true, true, false, false, false>,
            (true, false, true, true, true) => block_data_transfer::<T, true, false, true, true, true>,
            (true, false, true, true, false) => block_data_transfer::<T, true, false, true, true, false>,
            (true, false, true, false, true) => block_data_transfer::<T, true, false, true, false, true>,
            (true, false, true, false, false) => block_data_transfer::<T, true, false, true, false, false>,
            (true, false, false, true, true) => block_data_transfer::<T, true, false, false, true, true>,
            (true, false, false, true, false) => block_data_transfer::<T, true, false, false, true, false>,
            (true, false, false, false, true) => block_data_transfer::<T, true, false, false, false, true>,
            (true, false, false, false, false) => block_data_transfer::<T, true, false, false, false, false>,
            (false, true, true, true, true) => block_data_transfer::<T, false, true, true, true, true>,
            (false, true, true, true, false) => block_data_transfer::<T, false, true, true, true, false>,
            (false, true, true, false, true) => block_data_transfer::<T, false, true, true, false, true>,
            (false, true, true, false, false) => block_data_transfer::<T, false, true, true, false, false>,
            (false, true, false, true, true) => block_data_transfer::<T, false, true, false, true, true>,
            (false, true, false, true, false) => block_data_transfer::<T, false, true, false, true, false>,
            (false, true, false, false, true) => block_data_transfer::<T, false, true, false, false, true>,
            (false, true, false, false, false) => block_data_transfer::<T, false, true, false, false, false>,
            (false, false, true, true, true) => block_data_transfer::<T, false, false, true, true, true>,
            (false, false, true, true, false) => block_data_transfer::<T, false, false, true, true, false>,
            (false, false, true, false, true) => block_data_transfer::<T, false, false, true, false, true>,
            (false, false, true, false, false) => block_data_transfer::<T, false, false, true, false, false>,
            (false, false, false, true, true) => block_data_transfer::<T, false, false, false, true, true>,
            (false, false, false, true, false) => block_data_transfer::<T, false, false, false, true, false>,
            (false, false, false, false, true) => block_data_transfer::<T, false, false, false, false, true>,
            (false, false, false, false, false) => block_data_transfer::<T, false, false, false, false, false>,
        }
    } else if (instruction & 0b1111_0000_0000) == 0b1111_0000_0000 {
        software_interrupt
    } else {
        undefined_arm
    } 
}

#[rustfmt::skip]
const fn generate_arm_halfword_transfer<T: BusInterface, const IS_IMMEDIATE: bool>(instruction: usize) -> ArmHandler<T> {
    use crate::arm::opcode_tables::arm_handlers::halfword_and_signed_data_transfer;

    let pre_indexing = (instruction >> 8) & 1 == 1;
    let increment = (instruction >> 7) & 1 == 1;
    let write_back = (instruction >> 5) & 1 == 1;
    let load = (instruction >> 4) & 1 == 1;
    let s = (instruction >> 2) & 1 == 1;
    let h = (instruction >> 1) & 1 == 1;

    match (pre_indexing, increment, write_back, load, s, h) {
        (true, true, true, true, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, true, true, true, true>,
        (true, true, true, true, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, true, true, true, false>,
        (true, true, true, true, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, true, true, false, true>,
        (true, true, true, true, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, true, true, false, false>,
        (true, true, true, false, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, true, false, true, true>,
        (true, true, true, false, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, true, false, true, false>,
        (true, true, true, false, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, true, false, false, true>,
        (true, true, true, false, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, true, false, false, false>,
        (true, true, false, true, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, false, true, true, true>,
        (true, true, false, true, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, false, true, true, false>,
        (true, true, false, true, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, false, true, false, true>,
        (true, true, false, true, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, false, true, false, false>,
        (true, true, false, false, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, false, false, true, true>,
        (true, true, false, false, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, false, false, true, false>,
        (true, true, false, false, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, false, false, false, true>,
        (true, true, false, false, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, true, false, false, false, false>,
        (true, false, true, true, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, true, true, true, true>,
        (true, false, true, true, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, true, true, true, false>,
        (true, false, true, true, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, true, true, false, true>,
        (true, false, true, true, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, true, true, false, false>,
        (true, false, true, false, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, true, false, true, true>,
        (true, false, true, false, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, true, false, true, false>,
        (true, false, true, false, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, true, false, false, true>,
        (true, false, true, false, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, true, false, false, false>,
        (true, false, false, true, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, false, true, true, true>,
        (true, false, false, true, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, false, true, true, false>,
        (true, false, false, true, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, false, true, false, true>,
        (true, false, false, true, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, false, true, false, false>,
        (true, false, false, false, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, false, false, true, true>,
        (true, false, false, false, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, false, false, true, false>,
        (true, false, false, false, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, false, false, false, true>,
        (true, false, false, false, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, true, false, false, false, false, false>,
        (false, true, true, true, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, true, true, true, true>,
        (false, true, true, true, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, true, true, true, false>,
        (false, true, true, true, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, true, true, false, true>,
        (false, true, true, true, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, true, true, false, false>,
        (false, true, true, false, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, true, false, true, true>,
        (false, true, true, false, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, true, false, true, false>,
        (false, true, true, false, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, true, false, false, true>,
        (false, true, true, false, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, true, false, false, false>,
        (false, true, false, true, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, false, true, true, true>,
        (false, true, false, true, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, false, true, true, false>,
        (false, true, false, true, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, false, true, false, true>,
        (false, true, false, true, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, false, true, false, false>,
        (false, true, false, false, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, false, false, true, true>,
        (false, true, false, false, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, false, false, true, false>,
        (false, true, false, false, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, false, false, false, true>,
        (false, true, false, false, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, true, false, false, false, false>,
        (false, false, true, true, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, true, true, true, true>,
        (false, false, true, true, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, true, true, true, false>,
        (false, false, true, true, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, true, true, false, true>,
        (false, false, true, true, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, true, true, false, false>,
        (false, false, true, false, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, true, false, true, true>,
        (false, false, true, false, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, true, false, true, false>,
        (false, false, true, false, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, true, false, false, true>,
        (false, false, true, false, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, true, false, false, false>,
        (false, false, false, true, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, false, true, true, true>,
        (false, false, false, true, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, false, true, true, false>,
        (false, false, false, true, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, false, true, false, true>,
        (false, false, false, true, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, false, true, false, false>,
        (false, false, false, false, true, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, false, false, true, true>,
        (false, false, false, false, true, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, false, false, true, false>,
        (false, false, false, false, false, true) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, false, false, false, true>,
        (false, false, false, false, false, false) => halfword_and_signed_data_transfer::<T, IS_IMMEDIATE, false, false, false, false, false, false>,
    }
}
