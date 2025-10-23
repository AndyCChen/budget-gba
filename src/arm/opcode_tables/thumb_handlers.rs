use super::common::arithmetic;
use crate::arm::core::Arm7tdmi;

// const SP: u32 = 8; // stack pointer register number
// const LR: u32 = 9; // link register register number
// const PC: u32 = 10; // program counter register number

pub fn move_shifted<const SHIFT_OP: u8>(cpu: &mut Arm7tdmi, opcode: u16) {
    const LSL: u8 = 0;
    const LSR: u8 = 1;
    const ASR: u8 = 2;

    cpu.registers.r15 += 2;

    let rd = opcode & 0x7;
    let rs = (opcode >> 3) & 0x7;
    let shift_amount = (opcode >> 6) & 0x1F;

    let (result, carry_from_shift) = {
        let value = cpu.get_banked_register_thumb(rs);

        match SHIFT_OP {
            LSL => arithmetic::lsl(cpu, value, shift_amount.into()),
            LSR => arithmetic::lsr(cpu, true, value, shift_amount.into()),
            ASR => arithmetic::asr(cpu, true, value, shift_amount.into()),
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

pub fn mov_cmp_add_sub_immediate<const OP: u8>(cpu: &mut Arm7tdmi, opcode: u16) {
    const MOV: u8 = 0;
    const CMP: u8 = 1;
    const ADD: u8 = 2;
    const SUB: u8 = 3;

    cpu.registers.r15 += 2;

    let rd = (opcode >> 8) & 0x7;
    let immediate_value: u32 = (opcode & 0xFF).into();

    let op1 = cpu.get_banked_register_thumb(rd);

    let result = match OP {
        MOV => Some(arithmetic::mov::<true>(
            cpu,
            immediate_value,
            cpu.status.cpsr.c(),
        )),
        CMP => {
            arithmetic::sub::<true>(cpu, op1, immediate_value);
            None
        }
        ADD => Some(arithmetic::add::<true>(cpu, op1, immediate_value)),
        SUB => Some(arithmetic::sub::<true>(cpu, op1, immediate_value)),
        _ => panic!("Invalid OP! {OP}"),
    };

    if let Some(value) = result {
        cpu.set_banked_register_thumb(rd, value);
    }
}

pub fn undefined_thumb(_cpu: &mut Arm7tdmi, opcode: u16) {
    todo!("handle undefined opcode: {opcode}");
}
