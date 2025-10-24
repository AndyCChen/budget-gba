use super::common::arithmetic::*;
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
            LSL => lsl(cpu, value, shift_amount.into()),
            LSR => lsr(cpu, true, value, shift_amount.into()),
            ASR => asr(cpu, true, value, shift_amount.into()),
            _ => panic!("Invalid shift op!"),
        }
    };

    let result = mov::<true>(cpu, result, carry_from_shift);
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
        sub::<true>(cpu, op1, op2)
    } else {
        add::<true>(cpu, op1, op2)
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
        MOV => Some(mov::<true>(cpu, immediate_value, cpu.status.cpsr.c())),
        CMP => {
            sub::<true>(cpu, op1, immediate_value);
            None
        }
        ADD => Some(add::<true>(cpu, op1, immediate_value)),
        SUB => Some(sub::<true>(cpu, op1, immediate_value)),
        _ => panic!("Invalid OP! {OP}"),
    };

    if let Some(value) = result {
        cpu.set_banked_register_thumb(rd, value);
    }
}

mod alu_op {
    pub const AND: u8 = 0;
    pub const EOR: u8 = 1;
    pub const LSL: u8 = 2;
    pub const LSR: u8 = 3;
    pub const ASR: u8 = 4;
    pub const ADC: u8 = 5;
    pub const SBC: u8 = 6;
    pub const ROR: u8 = 7;
    pub const TST: u8 = 8;
    pub const NEG: u8 = 9;
    pub const CMP: u8 = 10;
    pub const CMN: u8 = 11;
    pub const ORR: u8 = 12;
    pub const MUL: u8 = 13;
    pub const BIC: u8 = 14;
    pub const MVN: u8 = 15;
}

fn mul(cpu: &mut Arm7tdmi, op1: u32, op2: u32) -> u32 {
    // todo handle extra i cycles
    let _i_cycles = 'block: {
        match op2 & 0xFFFF_FF00 {
            0xFFFF_FF00 | 0 => break 'block 1,
            _ => (),
        }

        match op2 & 0xFFFF_0000 {
            0xFFFF_0000 | 0 => break 'block 2,
            _ => (),
        }

        match op2 & 0xFF00_0000 {
            0xFF00_0000 | 0 => break 'block 3,
            _ => (),
        }

        4
    };

    let result = op1.wrapping_mul(op2);

    cpu.status.cpsr.set_n((result as i32).is_negative());
    cpu.status.cpsr.set_z(result == 0);

    result
}

pub fn alu_operations<const OP: u8>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rd = opcode & 0x7;
    let rs = (opcode >> 3) & 0x7;

    let op1 = cpu.get_banked_register_thumb(rd);
    let op2 = cpu.get_banked_register_thumb(rs);

    if matches!(OP, alu_op::LSL | alu_op::LSR | alu_op::ASR | alu_op::ROR) {
        // handle extra i cycle from register specified shift
    }

    let result = match OP {
        alu_op::AND => Some(and::<true>(cpu, op1, op2, cpu.status.cpsr.c())),
        alu_op::EOR => Some(eor::<true>(cpu, op1, op2, cpu.status.cpsr.c())),
        alu_op::LSL => {
            let (result, carry_from_shift) = lsl(cpu, op1, op2 & 0xFF);
            Some(mov::<true>(cpu, result, carry_from_shift))
        }
        alu_op::LSR => {
            let (result, carry_from_shift) = lsr(cpu, false, op1, op2 & 0xFF);
            Some(mov::<true>(cpu, result, carry_from_shift))
        }
        alu_op::ASR => {
            let (result, carry_from_shift) = asr(cpu, false, op1, op2 & 0xFF);
            Some(mov::<true>(cpu, result, carry_from_shift))
        }
        alu_op::ADC => Some(adc::<true>(cpu, op1, op2)),
        alu_op::SBC => Some(adc::<true>(cpu, op1, !op2)),
        alu_op::ROR => {
            let (result, carry_from_shift) = ror(cpu, false, op1, op2 & 0xFF);
            Some(mov::<true>(cpu, result, carry_from_shift))
        }
        alu_op::TST => {
            and::<true>(cpu, op1, op2, cpu.status.cpsr.c());
            None
        }
        alu_op::NEG => Some(sub::<true>(cpu, 0, op2)),
        alu_op::CMP => {
            sub::<true>(cpu, op1, op2);
            None
        }
        alu_op::CMN => {
            add::<true>(cpu, op1, op2);
            None
        }
        alu_op::ORR => Some(orr::<true>(cpu, op1, op2, cpu.status.cpsr.c())),
        alu_op::MUL => Some(mul(cpu, op1, op2)),
        alu_op::BIC => Some(and::<true>(cpu, op1, !op2, cpu.status.cpsr.c())),
        alu_op::MVN => Some(mov::<true>(cpu, !op2, cpu.status.cpsr.c())),
        _ => panic!("Invalid OP"),
    };

    if let Some(value) = result {
        cpu.set_banked_register_thumb(rd, value);
    }
}

pub fn undefined_thumb(_cpu: &mut Arm7tdmi, opcode: u16) {
    todo!("handle undefined opcode: {opcode}");
}
