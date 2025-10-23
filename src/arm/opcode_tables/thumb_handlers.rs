use super::common::arithmetic;
use crate::arm::core::Arm7tdmi;

// const SP: u32 = 8; // stack pointer register number
// const LR: u32 = 9; // link register register number
// const PC: u32 = 10; // program counter register number

pub fn move_shifted<const SHIFT_OP: u8>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rd = opcode & 0x7;
    let rs = (opcode >> 3) & 0x7;
    let shift_amount = (opcode >> 6) & 0x1F;

    let (result, carry_from_shift) = {
        let value = cpu.get_banked_register_thumb(rs);

        match SHIFT_OP {
            0 => arithmetic::lsl(cpu, value, shift_amount.into()),
            1 => arithmetic::lsr(cpu, true, value, shift_amount.into()),
            2 => arithmetic::asr(cpu, true, value, shift_amount.into()),
            _ => panic!("Invalid shift op!"),
        }
    };

    let result = arithmetic::mov::<true>(cpu, result, carry_from_shift);
    cpu.set_banked_register_thumb(rd, result);
}

pub fn add_subtract<const IMM: bool, const IS_SUBTRACT: bool>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rd = opcode & 0x7; // destination register
    let rs = (opcode >> 3) & 0x7; // source register
    let rn = (opcode >> 6) & 0x7; // 3 bit immediate value or register id

    let op1 = cpu.get_banked_register_thumb(rs);
    let op2: u32 = if IMM {
        rn.into()
    } else {
        cpu.get_banked_register_thumb(rn)
    };

    let result = if IS_SUBTRACT {
        arithmetic::sub::<true>(cpu, op1, op2)
    } else {
        arithmetic::add::<true>(cpu, op1, op2)
    };

    cpu.set_banked_register_thumb(rd, result);
}

pub fn undefined_thumb(_cpu: &mut Arm7tdmi, opcode: u16) {
    todo!("handle undefined opcode: {opcode}");
}
