pub mod arithmetic {
    use crate::arm::core::Arm7tdmi;

    // shift type constants for alu op

    pub const LSL: u8 = 0;
    pub const LSR: u8 = 1;
    pub const ASR: u8 = 2;
    pub const ROR: u8 = 3;

    fn update_flags_logical(cpu: &mut Arm7tdmi, result: u32, carry_from_shift: bool) {
        cpu.status.cpsr.set_c(carry_from_shift);
        cpu.status.cpsr.set_z(result == 0);
        cpu.status.cpsr.set_n((result as i32).is_negative());
    }

    fn update_flags_arithmetic(cpu: &mut Arm7tdmi, result: u32, carry: bool, overflow: bool) {
        cpu.status.cpsr.set_c(carry);
        cpu.status.cpsr.set_v(overflow);
        cpu.status.cpsr.set_z(result == 0);
        cpu.status.cpsr.set_n((result as i32).is_negative());
    }

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

    pub fn and<const SET_COND: bool>(
        cpu: &mut Arm7tdmi,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> u32 {
        let result = op1 & op2;

        if SET_COND {
            update_flags_logical(cpu, result, carry_from_shift);
        }

        result
    }

    pub fn eor<const SET_COND: bool>(
        cpu: &mut Arm7tdmi,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> u32 {
        let result = op1 ^ op2;

        if SET_COND {
            update_flags_logical(cpu, result, carry_from_shift);
        }

        result
    }

    pub fn sub<const SET_COND: bool>(cpu: &mut Arm7tdmi, op1: u32, op2: u32) -> u32 {
        let (result, carry) = op1.overflowing_sub(op2);
        let overflow = ((result ^ op1) & (result ^ !op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, !carry, overflow);
        }

        result
    }

    pub fn add<const SET_COND: bool>(cpu: &mut Arm7tdmi, op1: u32, op2: u32) -> u32 {
        let (result, carry) = op1.overflowing_add(op2);
        let overflow = ((result ^ op1) & (result ^ op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, carry, overflow);
        }

        result
    }

    pub fn adc<const SET_COND: bool>(cpu: &mut Arm7tdmi, op1: u32, op2: u32) -> u32 {
        let (result, carry) = {
            let (op2_with_carry, carry0) = op2.overflowing_add(u32::from(cpu.status.cpsr.c()));
            let (result, carry1) = op1.overflowing_add(op2_with_carry);
            (result, carry0 || carry1)
        };
        let overflow = ((result ^ op1) & (result ^ op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, carry, overflow);
        }

        result
    }

    pub fn orr<const SET_COND: bool>(
        cpu: &mut Arm7tdmi,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> u32 {
        let result = op1 | op2;

        if SET_COND {
            update_flags_logical(cpu, result, carry_from_shift);
        }

        result
    }

    pub fn mov<const SET_COND: bool>(
        cpu: &mut Arm7tdmi,
        value_to_move: u32,
        carry_from_shift: bool,
    ) -> u32 {
        if SET_COND {
            update_flags_logical(cpu, value_to_move, carry_from_shift);
        }

        value_to_move
    }
}

pub mod reg_constant {
    pub const STACK_POINTER: u32 = 13;
    pub const LINK_REGISTER: u32 = 14;
}
