use crate::arm::core::{Arm7tdmi, Mode, StatusRegister};

pub fn branch_and_exchange(cpu: &mut Arm7tdmi, opcode: u32) {
    let branch_address = cpu.get_register_arm(opcode & 0xF);
    let is_thumb_mode = (branch_address & 0x1) == 1;
    cpu.status.cpsr.set_t(is_thumb_mode);

    cpu.registers.r15 = branch_address; // pc is updated so we need to refill instruction pipeline

    if is_thumb_mode {
        cpu.registers.r15 &= !1;
        cpu.pipeline_refill_thumb();
    } else {
        cpu.pipeline_refill_arm();
    }
}

pub fn branch_and_link<const LINK: bool>(cpu: &mut Arm7tdmi, opcode: u32) {
    let mut offset = (opcode & 0xFFFFFF) << 2;

    // branch with link, save r15 (pc) to r14 (link register)
    if LINK {
        cpu.set_register_arm(14, cpu.registers.r15 - 4);
    }

    // positive
    if opcode & (1 << 23) == 0 {
        cpu.registers.r15 = cpu.registers.r15.wrapping_add(offset);
    }
    // negative
    else {
        offset |= 0xFC_000000;
        offset = !offset + 1;
        cpu.registers.r15 = cpu.registers.r15.wrapping_sub(offset);
    }

    cpu.pipeline_refill_arm();
}

pub mod data_op {
    use crate::arm::core::Arm7tdmi;

    // data op constants

    pub const AND: u8 = 0;
    pub const EOR: u8 = 1;
    pub const SUB: u8 = 2;
    pub const RSB: u8 = 3;
    pub const ADD: u8 = 4;
    pub const ADC: u8 = 5;
    pub const SBC: u8 = 6;
    pub const RSC: u8 = 7;
    pub const TST: u8 = 8;
    pub const TEQ: u8 = 9;
    pub const CMP: u8 = 10;
    pub const CMN: u8 = 11;
    pub const ORR: u8 = 12;
    pub const MOV: u8 = 13;
    pub const BIC: u8 = 14;
    pub const MVN: u8 = 15;

    // shift type constants

    pub const LSL: u8 = 0;
    pub const LSR: u8 = 1;
    pub const ASR: u8 = 2;
    pub const ROR: u8 = 3;

    #[inline]
    fn update_flags_logical(cpu: &mut Arm7tdmi, result: u32, carry_from_shift: bool) {
        cpu.status.cpsr.set_c(carry_from_shift);
        cpu.status.cpsr.set_z(result == 0);
        cpu.status.cpsr.set_n((result & 0x8000_0000) != 0);
    }

    #[inline]
    fn update_flags_arithmetic(cpu: &mut Arm7tdmi, result: u32, carry: bool, overflow: bool) {
        cpu.status.cpsr.set_c(carry);
        cpu.status.cpsr.set_v(overflow);
        cpu.status.cpsr.set_z(result == 0);
        cpu.status.cpsr.set_n((result & 0x8000_0000) != 0);
    }

    #[inline]
    pub fn lsl(cpu: &Arm7tdmi, value_to_shift: u32, shift_amount: u32) -> (u32, bool) {
        if shift_amount == 0 {
            let carry_from_shift = cpu.status.cpsr.c();
            (value_to_shift, carry_from_shift)
        } else if shift_amount <= 32 {
            let carry_from_shift = (value_to_shift & (1 << (32 - shift_amount))) != 0;
            (
                value_to_shift.checked_shl(shift_amount).unwrap_or(0),
                carry_from_shift,
            )
        } else {
            (0, false)
        }
    }

    #[inline]
    pub fn lsr(
        cpu: &Arm7tdmi,
        is_immediate: bool,
        value_to_shift: u32,
        mut shift_amount: u32,
    ) -> (u32, bool) {
        if is_immediate && shift_amount == 0 {
            shift_amount = 32;
        }

        if shift_amount == 0 {
            let carry_from_shift = cpu.status.cpsr.c();
            (value_to_shift, carry_from_shift)
        } else if shift_amount <= 32 {
            let carry_from_shift = (value_to_shift & (1 << (shift_amount - 1))) != 0;
            (
                value_to_shift.checked_shr(shift_amount).unwrap_or(0),
                carry_from_shift,
            )
        } else {
            (0, false)
        }
    }

    #[inline]
    pub fn asr(
        cpu: &Arm7tdmi,
        is_immediate: bool,
        value_to_shift: u32,
        mut shift_amount: u32,
    ) -> (u32, bool) {
        if is_immediate && shift_amount == 0 {
            shift_amount = 32;
        }

        if shift_amount == 0 {
            let carry_from_shift = cpu.status.cpsr.c();
            (value_to_shift, carry_from_shift)
        } else if shift_amount < 32 {
            let carry_from_shift = (value_to_shift & (1 << (shift_amount - 1))) != 0;
            let result = ((value_to_shift as i32) >> shift_amount) as u32;
            (result, carry_from_shift)
        } else {
            let carry_from_shift = (value_to_shift & 0x8000_0000) != 0;
            let result = if (value_to_shift as i32).is_negative() {
                u32::MAX
            } else {
                0
            };
            (result, carry_from_shift)
        }
    }

    #[inline]
    pub fn ror(
        cpu: &Arm7tdmi,
        is_immediate: bool,
        value_to_shift: u32,
        shift_amount: u32,
    ) -> (u32, bool) {
        if shift_amount == 0 {
            if is_immediate {
                rrx(cpu, value_to_shift)
            } else {
                let carry_from_shift = cpu.status.cpsr.c();
                (value_to_shift, carry_from_shift)
            }
        } else {
            let shift_amount = shift_amount - (u32::BITS * (shift_amount.div_ceil(u32::BITS) - 1));
            let carry_from_shift = (value_to_shift & (1 << (shift_amount - 1))) != 0;

            (value_to_shift.rotate_right(shift_amount), carry_from_shift)
        }
    }

    #[inline]
    pub fn rrx(cpu: &Arm7tdmi, value_to_shift: u32) -> (u32, bool) {
        let carry_in = u32::from(cpu.status.cpsr.c()) << 31;
        let carry_out = (value_to_shift & 1) != 0;
        let result = carry_in | (value_to_shift >> 1);

        (result, carry_out)
    }

    // rd: destination register for instruction that write back a result
    // op1: 1st operand
    // op2: 2nd operand if any
    // carry_from_shift: carry bit from barrel shifter for logical bit ops

    pub fn and<const SET_COND: bool, const WRITE_BACK: bool>(
        cpu: &mut Arm7tdmi,
        rd: u32,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> bool {
        let result = op1 & op2;

        if SET_COND {
            update_flags_logical(cpu, result, carry_from_shift);
        }

        if WRITE_BACK {
            cpu.set_register_arm(rd, result);
        }

        WRITE_BACK
    }

    pub fn eor<const SET_COND: bool, const WRITE_BACK: bool>(
        cpu: &mut Arm7tdmi,
        rd: u32,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> bool {
        let result = op1 ^ op2;

        if SET_COND {
            update_flags_logical(cpu, result, carry_from_shift);
        }

        if WRITE_BACK {
            cpu.set_register_arm(rd, result);
        }

        WRITE_BACK
    }

    pub fn sub<const SET_COND: bool, const WRITE_BACK: bool>(
        cpu: &mut Arm7tdmi,
        rd: u32,
        op1: u32,
        op2: u32,
    ) -> bool {
        let (result, carry) = op1.overflowing_sub(op2);
        let overflow = ((result ^ op1) & (result ^ !op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, !carry, overflow);
        }

        if WRITE_BACK {
            cpu.set_register_arm(rd, result);
        }

        WRITE_BACK
    }

    pub fn add<const SET_COND: bool, const WRITE_BACK: bool>(
        cpu: &mut Arm7tdmi,
        rd: u32,
        op1: u32,
        op2: u32,
    ) -> bool {
        let (result, carry) = op1.overflowing_add(op2);
        let overflow = ((result ^ op1) & (result ^ op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, carry, overflow);
        }

        if WRITE_BACK {
            cpu.set_register_arm(rd, result);
        }

        WRITE_BACK
    }

    pub fn adc<const SET_COND: bool, const WRITE_BACK: bool>(
        cpu: &mut Arm7tdmi,
        rd: u32,
        op1: u32,
        op2: u32,
    ) -> bool {
        let (result, carry) = {
            let (op2_with_carry, carry0) = op2.overflowing_add(u32::from(cpu.status.cpsr.c()));
            let (result, carry1) = op1.overflowing_add(op2_with_carry);
            (result, carry0 || carry1)
        };
        let overflow = ((result ^ op1) & (result ^ op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, carry, overflow);
        }

        if WRITE_BACK {
            cpu.set_register_arm(rd, result);
        }

        WRITE_BACK
    }

    pub fn sbc<const SET_COND: bool, const WRITE_BACK: bool>(
        cpu: &mut Arm7tdmi,
        rd: u32,
        op1: u32,
        op2: u32,
    ) -> bool {
        let (op2, carry0) = (!op2).overflowing_add(u32::from(cpu.status.cpsr.c()));
        let (result, carry1) = op1.overflowing_add(op2);
        let overflow = ((result ^ op1) & (result ^ !op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, carry0 || carry1, overflow);
        }

        if WRITE_BACK {
            cpu.set_register_arm(rd, result);
        }

        WRITE_BACK
    }

    pub fn orr<const SET_COND: bool, const WRITE_BACK: bool>(
        cpu: &mut Arm7tdmi,
        rd: u32,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> bool {
        let result = op1 | op2;

        if SET_COND {
            update_flags_logical(cpu, result, carry_from_shift);
        }

        if WRITE_BACK {
            cpu.set_register_arm(rd, result);
        }

        WRITE_BACK
    }

    pub fn mov<const SET_COND: bool, const WRITE_BACK: bool>(
        cpu: &mut Arm7tdmi,
        rd: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> bool {
        if SET_COND {
            update_flags_logical(cpu, op2, carry_from_shift);
        }

        if WRITE_BACK {
            cpu.set_register_arm(rd, op2);
        }

        WRITE_BACK
    }
}

pub fn data_processing<
    const IMM: bool,
    const DATA_OP: u8,
    const SET_COND: bool,
    const SHIFT: u8,
>(
    cpu: &mut Arm7tdmi,
    opcode: u32,
) {
    use self::data_op::*;

    let rn = (opcode >> 16) & 0xF; // 1st operand register
    let rd = (opcode >> 12) & 0xF; // destination register of result

    // shift amount for operand 2 is specified by a register
    let register_specified_shift = const { !IMM && (SHIFT & 1) != 0 };

    let (op2, carry_from_shift) = if IMM {
        let shift_amount = ((opcode >> 8) & 0xF) * 2;
        let value_to_shift = opcode & 0xFF;

        if shift_amount == 0 {
            let carry_from_shift = cpu.status.cpsr.c();
            (value_to_shift, carry_from_shift)
        } else {
            let carry_from_shift = (value_to_shift & (1 << (shift_amount - 1))) != 0;
            (value_to_shift.rotate_right(shift_amount), carry_from_shift)
        }
    } else {
        let rm = opcode & 0xF; // register to apply shift operation on
        let shift_field = (opcode >> 4) & 0xFF;
        let is_immediate = !register_specified_shift;

        // shift via 5-bit unsigned value
        let shift_amount = if is_immediate {
            shift_field >> 3
        }
        // shift via bottom byte in register Rs
        else {
            let rs_value = cpu.get_register_arm(shift_field >> 4) & 0xFF;

            // pc is ahead by 12 when a register specified shift is used
            if register_specified_shift {
                // todo handle extra I cycle

                cpu.registers.r15 += 4;
            }

            rs_value
        };

        let rm_value = cpu.get_register_arm(rm);
        let shift_type = const { (SHIFT >> 1) & 0x3 };
        match shift_type {
            LSL => lsl(cpu, rm_value, shift_amount),
            LSR => lsr(cpu, is_immediate, rm_value, shift_amount),
            ASR => asr(cpu, is_immediate, rm_value, shift_amount),
            ROR => ror(cpu, is_immediate, rm_value, shift_amount),
            _ => panic!("Invalid shift type!"),
        }
    };
    let op1 = cpu.get_register_arm(rn);

    if !register_specified_shift {
        cpu.registers.r15 += 4;
    }

    let is_write_back = match DATA_OP {
        AND => and::<SET_COND, true>(cpu, rd, op1, op2, carry_from_shift),
        EOR => eor::<SET_COND, true>(cpu, rd, op1, op2, carry_from_shift),
        SUB => sub::<SET_COND, true>(cpu, rd, op1, op2),
        RSB => sub::<SET_COND, true>(cpu, rd, op2, op1),
        ADD => add::<SET_COND, true>(cpu, rd, op1, op2),
        ADC => adc::<SET_COND, true>(cpu, rd, op1, op2),
        SBC => adc::<SET_COND, true>(cpu, rd, op1, !op2),
        RSC => adc::<SET_COND, true>(cpu, rd, op2, !op1),
        TST => and::<true, false>(cpu, rd, op1, op2, carry_from_shift),
        TEQ => eor::<true, false>(cpu, rd, op1, op2, carry_from_shift),
        CMP => sub::<true, false>(cpu, rd, op1, op2),
        CMN => add::<true, false>(cpu, rd, op1, op2),
        ORR => orr::<SET_COND, true>(cpu, rd, op1, op2, carry_from_shift),
        MOV => mov::<SET_COND, true>(cpu, rd, op2, carry_from_shift),
        BIC => and::<SET_COND, true>(cpu, rd, op1, !op2, carry_from_shift),
        MVN => mov::<SET_COND, true>(cpu, rd, !op2, carry_from_shift),
        _ => panic!("Invalid data op! {DATA_OP}"),
    };

    if rd == 15 {
        // move spsr into cpsr if r15 is used as a destination and S is set
        if SET_COND {
            cpu.status.cpsr = StatusRegister::from_bits(cpu.get_spsr());
        }

        if is_write_back {
            cpu.pipeline_refill_arm(); // pc updated, so refill pipeline
        }
    }
}

// poor man's macro to help generate data proc instructions for the arm lookup table at compile time

#[macro_export]
macro_rules! data_processing {
    ($imm:expr, $data_opcode:expr, $set_cond:expr, $shift:expr) => {
        match $data_opcode {
            data_op::AND => _data_processing_inner!($imm, { data_op::AND }, $set_cond, $shift),
            data_op::EOR => _data_processing_inner!($imm, { data_op::EOR }, $set_cond, $shift),
            data_op::SUB => _data_processing_inner!($imm, { data_op::SUB }, $set_cond, $shift),
            data_op::RSB => _data_processing_inner!($imm, { data_op::RSB }, $set_cond, $shift),
            data_op::ADD => _data_processing_inner!($imm, { data_op::ADD }, $set_cond, $shift),
            data_op::ADC => _data_processing_inner!($imm, { data_op::ADC }, $set_cond, $shift),
            data_op::SBC => _data_processing_inner!($imm, { data_op::SBC }, $set_cond, $shift),
            data_op::RSC => _data_processing_inner!($imm, { data_op::RSC }, $set_cond, $shift),
            data_op::TST => _data_processing_inner!($imm, { data_op::TST }, $set_cond, $shift),
            data_op::TEQ => _data_processing_inner!($imm, { data_op::TEQ }, $set_cond, $shift),
            data_op::CMP => _data_processing_inner!($imm, { data_op::CMP }, $set_cond, $shift),
            data_op::CMN => _data_processing_inner!($imm, { data_op::CMN }, $set_cond, $shift),
            data_op::ORR => _data_processing_inner!($imm, { data_op::ORR }, $set_cond, $shift),
            data_op::MOV => _data_processing_inner!($imm, { data_op::MOV }, $set_cond, $shift),
            data_op::BIC => _data_processing_inner!($imm, { data_op::BIC }, $set_cond, $shift),
            data_op::MVN => _data_processing_inner!($imm, { data_op::MVN }, $set_cond, $shift),
            _ => panic!("Invalid data op!"),
        }
    };
}

#[macro_export]
macro_rules! _data_processing_inner {
    ($imm:expr, $data_opcode:expr, $set_cond:expr, $shift:expr) => {
        match $shift {
            0 => data_processing::<$imm, $data_opcode, $set_cond, 0>,
            1 => data_processing::<$imm, $data_opcode, $set_cond, 1>,
            2 => data_processing::<$imm, $data_opcode, $set_cond, 2>,
            3 => data_processing::<$imm, $data_opcode, $set_cond, 3>,
            4 => data_processing::<$imm, $data_opcode, $set_cond, 4>,
            5 => data_processing::<$imm, $data_opcode, $set_cond, 5>,
            6 => data_processing::<$imm, $data_opcode, $set_cond, 6>,
            7 => data_processing::<$imm, $data_opcode, $set_cond, 7>,
            8 => data_processing::<$imm, $data_opcode, $set_cond, 8>,
            9 => data_processing::<$imm, $data_opcode, $set_cond, 9>,
            10 => data_processing::<$imm, $data_opcode, $set_cond, 10>,
            11 => data_processing::<$imm, $data_opcode, $set_cond, 11>,
            12 => data_processing::<$imm, $data_opcode, $set_cond, 12>,
            13 => data_processing::<$imm, $data_opcode, $set_cond, 13>,
            14 => data_processing::<$imm, $data_opcode, $set_cond, 14>,
            15 => data_processing::<$imm, $data_opcode, $set_cond, 15>,
            _ => panic!("shift field must be in range 0-15!"),
        }
    };
}

pub fn mrs<const SPSR_DEST: bool>(cpu: &mut Arm7tdmi, opcode: u32) {
    let rd = (opcode >> 12) & 0xF; // destination register

    if SPSR_DEST {
        cpu.set_register_arm(rd, cpu.get_spsr());
    } else {
        cpu.set_register_arm(rd, cpu.status.cpsr.into_bits());
    }

    cpu.registers.r15 += 4;
}

pub fn msr<const IMM: bool, const SPSR_DEST: bool>(cpu: &mut Arm7tdmi, opcode: u32) {
    let mut mask: u32 = 0;

    // control field: bits 7-0
    if ((opcode >> 16) & 1) != 0 {
        mask |= 0x0000_00FF;
    }

    // extension field: bits 15-8
    if ((opcode >> 17) & 1) != 0 {
        mask |= 0x0000_FF00;
    }

    // status field: bits 23-16
    if ((opcode >> 18) & 1) != 0 {
        mask |= 0x00FF_0000;
    }

    // flag field: bits 31-24
    if ((opcode >> 19) & 1) != 0 {
        mask |= 0xFF00_0000;
    }

    let mut transfer_value = if IMM {
        let immediate_value = opcode & 0xFF;
        let rotate_by = ((opcode >> 8) & 0xF) * 2;

        immediate_value.rotate_right(rotate_by)
    } else {
        let rm = opcode & 0xF; // source register
        cpu.get_register_arm(rm)
    };

    let mut psr_value = if SPSR_DEST {
        cpu.get_spsr()
    } else {
        cpu.status.cpsr.into_bits()
    };

    if !SPSR_DEST {
        // bit 4 of control field is always 1, this isn't documented in the arm7tdmi data sheet for some reason
        transfer_value |= 0b1_0000;

        // when in user mode, only flag field of cpsr can be updated
        if cpu.status.cpsr.mode_bits() == Mode::User {
            mask &= 0xFF00_0000;
        }
    }

    psr_value = (psr_value & !mask) | (transfer_value & mask);

    if SPSR_DEST {
        cpu.set_spsr(psr_value);
    } else {
        cpu.status.cpsr = StatusRegister::from_bits(psr_value);
    }

    cpu.registers.r15 += 4;
}

pub fn undefined_arm(_cpu: &mut Arm7tdmi, opcode: u32) {
    todo!("handle undefined opcode: {opcode}");
}
