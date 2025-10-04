use crate::arm::core::{Arm7tdmi, StatusRegister};

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
        let (op2, carry0) = op2.overflowing_add(u32::from(cpu.status.cpsr.c()));
        let (result, carry1) = op1.overflowing_add(op2);
        let overflow = ((result ^ op1) & (result ^ op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, carry0 || carry1, overflow);
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

pub fn data_processing<const IMM: bool, const OP_CODE: u8, const SET_COND: bool>(
    cpu: &mut Arm7tdmi,
    opcode: u32,
) {
    use self::data_op::*;

    let carry_from_shift: bool;

    let op1 = cpu.get_register_arm((opcode >> 16) & 0xF);
    let op2 = if IMM {
        let shift_amount = ((opcode >> 8) & 0xF) * 2;
        let value_to_shift = opcode & 0xFF;

        if shift_amount == 0 {
            carry_from_shift = cpu.status.cpsr.c();
            value_to_shift
        } else {
            carry_from_shift = (value_to_shift & (1 << (shift_amount - 1))) != 0;
            value_to_shift.rotate_right(shift_amount)
        }
    } else {
        let rm = cpu.get_register_arm(opcode & 0xF); // register value to shift
        let shift = (opcode >> 4) & 0xFF;

        // shift via 5-bit unsigned value
        let shift_amount = if (shift & 0x1) == 0 {
            shift >> 3
        }
        // shift via bottom byte in register Rs
        else {
            assert_ne!(
                shift >> 4,
                15,
                "r15 should not be used as Rs to specify shift amount!"
            );
            cpu.get_register_arm(shift >> 4) & 0xFF
        };

        match (shift >> 1) & 0x3 {
            // LSL
            0b00 => {
                if shift_amount == 0 {
                    carry_from_shift = cpu.status.cpsr.c();
                    rm
                } else if shift_amount <= 32 {
                    carry_from_shift = (rm & (1 << (32 - shift_amount))) != 0;
                    rm.checked_shl(shift_amount).unwrap_or(0)
                } else {
                    carry_from_shift = false;
                    0
                }
            }
            // LSR
            0b01 => {
                if shift_amount == 0 {
                    carry_from_shift = cpu.status.cpsr.c();
                    rm
                } else if shift_amount <= 32 {
                    carry_from_shift = (rm & (1 << (shift_amount - 1))) != 0;
                    rm.checked_shr(shift_amount).unwrap_or(0)
                } else {
                    carry_from_shift = false;
                    0
                }
            }
            // ASR
            0b10 => {
                if shift_amount == 0 {
                    carry_from_shift = cpu.status.cpsr.c();
                    rm
                } else if shift_amount < 32 {
                    carry_from_shift = (rm & (1 << (shift_amount - 1))) != 0;
                    ((rm as i32) >> shift_amount) as u32
                } else {
                    carry_from_shift = (rm & 0x8000_0000) != 0;
                    if (rm as i32).is_negative() {
                        u32::MAX
                    } else {
                        0
                    }
                }
            }
            // ROR
            0b11 => {
                if shift_amount == 0 {
                    carry_from_shift = cpu.status.cpsr.c();
                    rm
                } else {
                    let shift_amount =
                        shift_amount - (u32::BITS * (shift_amount.div_ceil(u32::BITS) - 1));

                    carry_from_shift = (rm & (1 << (shift_amount - 1))) != 0;
                    rm.rotate_right(shift_amount)
                }
            }
            _ => panic!("Invalid shift type!"),
        }
    };

    cpu.registers.r15 += 4;

    // destination register of result
    let rd = (opcode >> 12) & 0xF;

    let is_write_back = match OP_CODE {
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

        _ => panic!("Invalid data op! {OP_CODE}"),
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

pub fn undefined_arm(_cpu: &mut Arm7tdmi, opcode: u32) {
    panic!("undefined opcode! {opcode}");
}
