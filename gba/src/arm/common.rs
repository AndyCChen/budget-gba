pub mod arithmetic {
    use crate::arm::core::StatusRegister;

    // shift type constants for alu op

    pub const LSL: u8 = 0;
    pub const LSR: u8 = 1;
    pub const ASR: u8 = 2;
    pub const ROR: u8 = 3;

    pub fn lsl(cpsr: StatusRegister, value_to_shift: u32, shift_amount: u32) -> (u32, bool) {
        if shift_amount == 0 {
            let carry_from_shift = cpsr.c();
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
        cpsr: StatusRegister,
        is_immediate: bool,
        value_to_shift: u32,
        mut shift_amount: u32,
    ) -> (u32, bool) {
        if is_immediate && shift_amount == 0 {
            shift_amount = 32;
        }

        if shift_amount == 0 {
            let carry_from_shift = cpsr.c();
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
        cpsr: StatusRegister,
        is_immediate: bool,
        value_to_shift: u32,
        mut shift_amount: u32,
    ) -> (u32, bool) {
        if is_immediate && shift_amount == 0 {
            shift_amount = 32;
        }

        if shift_amount == 0 {
            let carry_from_shift = cpsr.c();
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
        cpsr: StatusRegister,
        is_immediate: bool,
        value_to_shift: u32,
        shift_amount: u32,
    ) -> (u32, bool) {
        if shift_amount == 0 {
            if is_immediate {
                rrx(cpsr, value_to_shift)
            } else {
                let carry_from_shift = cpsr.c();
                (value_to_shift, carry_from_shift)
            }
        } else {
            let shift_amount = shift_amount - (u32::BITS * (shift_amount.div_ceil(u32::BITS) - 1));
            let carry_from_shift = (value_to_shift & (1 << (shift_amount - 1))) != 0;

            (value_to_shift.rotate_right(shift_amount), carry_from_shift)
        }
    }

    fn rrx(cpsr: StatusRegister, value_to_shift: u32) -> (u32, bool) {
        let carry_in = u32::from(cpsr.c()) << 31;
        let carry_out = (value_to_shift & 1) != 0;
        let result = carry_in | (value_to_shift >> 1);

        (result, carry_out)
    }

    // rd: destination register for instruction that write back a result
    // op1: 1st operand
    // op2: 2nd operand if any
    // carry_from_shift: carry bit from barrel shifter for logical bit ops

    pub fn and<const SET_COND: bool>(
        cpsr: &mut StatusRegister,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> u32 {
        let result = op1 & op2;

        if SET_COND {
            update_flags_logical(cpsr, result, carry_from_shift);
        }

        result
    }

    pub fn eor<const SET_COND: bool>(
        cpsr: &mut StatusRegister,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> u32 {
        let result = op1 ^ op2;

        if SET_COND {
            update_flags_logical(cpsr, result, carry_from_shift);
        }

        result
    }

    pub fn sub<const SET_COND: bool>(cpsr: &mut StatusRegister, op1: u32, op2: u32) -> u32 {
        let (result, carry) = op1.overflowing_sub(op2);
        let overflow = ((result ^ op1) & (result ^ !op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpsr, result, !carry, overflow);
        }

        result
    }

    pub fn add<const SET_COND: bool>(cpsr: &mut StatusRegister, op1: u32, op2: u32) -> u32 {
        let (result, carry) = op1.overflowing_add(op2);
        let overflow = ((result ^ op1) & (result ^ op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpsr, result, carry, overflow);
        }

        result
    }

    pub fn adc<const SET_COND: bool>(cpsr: &mut StatusRegister, op1: u32, op2: u32) -> u32 {
        let (result, carry) = {
            let (op2_with_carry, carry0) = op2.overflowing_add(u32::from(cpsr.c()));
            let (result, carry1) = op1.overflowing_add(op2_with_carry);
            (result, carry0 || carry1)
        };
        let overflow = ((result ^ op1) & (result ^ op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpsr, result, carry, overflow);
        }

        result
    }

    pub fn orr<const SET_COND: bool>(
        cpsr: &mut StatusRegister,
        op1: u32,
        op2: u32,
        carry_from_shift: bool,
    ) -> u32 {
        let result = op1 | op2;

        if SET_COND {
            update_flags_logical(cpsr, result, carry_from_shift);
        }

        result
    }

    pub fn mov<const SET_COND: bool>(
        cpsr: &mut StatusRegister,
        value_to_move: u32,
        carry_from_shift: bool,
    ) -> u32 {
        if SET_COND {
            update_flags_logical(cpsr, value_to_move, carry_from_shift);
        }

        value_to_move
    }

    fn update_flags_logical(cpsr: &mut StatusRegister, result: u32, carry_from_shift: bool) {
        cpsr.set_c(carry_from_shift);
        cpsr.set_z(result == 0);
        cpsr.set_n((result as i32).is_negative());
    }

    fn update_flags_arithmetic(
        cpsr: &mut StatusRegister,
        result: u32,
        carry: bool,
        overflow: bool,
    ) {
        cpsr.set_c(carry);
        cpsr.set_v(overflow);
        cpsr.set_z(result == 0);
        cpsr.set_n((result as i32).is_negative());
    }
}

// data op constants for arm 32 data processing instructions
pub mod arm_data_op {
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
}

pub mod reg_constant {
    pub const STACK_POINTER: u32 = 13;
    pub const LINK_REGISTER: u32 = 14;
    pub const PROGRAM_COUNTER: u32 = 15;
}

use num_traits::{PrimInt, Unsigned, WrappingAdd};

/// Retrieve raw binary representation of a negative number as a unsigned integer
pub fn to_negative<T: Unsigned + PrimInt + WrappingAdd>(input: T) -> T {
    (!input).wrapping_add(&T::one())
}

#[cfg(test)]
mod test {
    use super::to_negative;

    #[test]
    fn test_negative() {
        assert_eq!(to_negative(5u8), (-5i8) as u8);
        assert_eq!(to_negative(0x100u16), (-256i16) as u16);
        assert_eq!(to_negative(0xFFFF), (-0xFFFFi32) as u32);
        assert_eq!(to_negative(128), (-128i64) as u64);
    }
}
