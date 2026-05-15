use crate::arm::constants::access_code;
use crate::arm::core::{Arm7tdmi, CpuMode::*, Mode, StatusRegister};
use crate::arm::opcode_tables::common::reg_constant::*;
use std::num::Wrapping;

pub fn branch_and_exchange(cpu: &mut Arm7tdmi, opcode: u32) {
    let branch_address = cpu.get_banked_register(opcode & 0xF);
    let mode = if (branch_address & 0x1) == 0 {
        ArmMode
    } else {
        ThumbMode
    };
    cpu.status.cpsr.set_t(mode);

    cpu.registers.r15 = Wrapping(branch_address); // pc is updated so we need to refill instruction pipeline

    match mode {
        ArmMode => cpu.pipeline_refill_arm(),
        ThumbMode => {
            cpu.registers.r15 &= !1;
            cpu.pipeline_refill_thumb();
        }
    }
}

pub fn branch_and_link<const LINK: bool>(cpu: &mut Arm7tdmi, opcode: u32) {
    let mut offset = (opcode & 0xFFFFFF) << 2;

    // branch with link, save r15 (pc) to r14 (link register)
    if LINK {
        cpu.set_banked_register(LINK_REGISTER, (cpu.registers.r15 - Wrapping(4)).0);
    }

    // negative
    if opcode & (1 << 23) != 0 {
        offset |= 0xFC_000000;
    }

    cpu.registers.r15 += offset;
    cpu.pipeline_refill_arm();
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
    use super::common::arithmetic::*;
    use super::common::arm_data_op::*;

    let rn = (opcode >> 16) & 0xF; // 1st operand register
    let rd = (opcode >> 12) & 0xF; // destination register of result

    // shift amount for operand 2 is specified by a register
    let register_specified_shift = !IMM && (SHIFT & 1) != 0;

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
            let rs_value = cpu.get_banked_register(shift_field >> 4) & 0xFF;

            // pc is ahead by 12 when a register specified shift is used
            cpu.bus.i_cycle();
            cpu.pipeline_state = access_code::NONSEQUENTIAL | access_code::CODE;
            cpu.registers.r15 += 4;

            rs_value
        };

        let rm_value = cpu.get_banked_register(rm);
        let shift_type = const { (SHIFT >> 1) & 0x3 };
        match shift_type {
            LSL => lsl(cpu, rm_value, shift_amount),
            LSR => lsr(cpu, is_immediate, rm_value, shift_amount),
            ASR => asr(cpu, is_immediate, rm_value, shift_amount),
            ROR => ror(cpu, is_immediate, rm_value, shift_amount),
            _ => panic!("Invalid shift type!"),
        }
    };
    let op1 = cpu.get_banked_register(rn);

    if !register_specified_shift {
        cpu.pipeline_state = access_code::SEQUENTIAL | access_code::CODE;
        cpu.registers.r15 += 4;
    }

    let result = match DATA_OP {
        AND => Some(and::<SET_COND>(cpu, op1, op2, carry_from_shift)),
        EOR => Some(eor::<SET_COND>(cpu, op1, op2, carry_from_shift)),
        SUB => Some(sub::<SET_COND>(cpu, op1, op2)),
        RSB => Some(sub::<SET_COND>(cpu, op2, op1)),
        ADD => Some(add::<SET_COND>(cpu, op1, op2)),
        ADC => Some(adc::<SET_COND>(cpu, op1, op2)),
        SBC => Some(adc::<SET_COND>(cpu, op1, !op2)),
        RSC => Some(adc::<SET_COND>(cpu, op2, !op1)),
        TST => {
            and::<true>(cpu, op1, op2, carry_from_shift);
            None
        }
        TEQ => {
            eor::<true>(cpu, op1, op2, carry_from_shift);
            None
        }
        CMP => {
            sub::<true>(cpu, op1, op2);
            None
        }
        CMN => {
            add::<true>(cpu, op1, op2);
            None
        }
        ORR => Some(orr::<SET_COND>(cpu, op1, op2, carry_from_shift)),
        MOV => Some(mov::<SET_COND>(cpu, op2, carry_from_shift)),
        BIC => Some(and::<SET_COND>(cpu, op1, !op2, carry_from_shift)),
        MVN => Some(mov::<SET_COND>(cpu, !op2, carry_from_shift)),
        _ => panic!("Invalid data op! {DATA_OP}"),
    };

    if let Some(value) = result {
        cpu.set_banked_register(rd, value);
    }

    if rd == 15 {
        // move spsr into cpsr if r15 is used as a destination and S is set
        if SET_COND {
            cpu.status.cpsr = StatusRegister::from_bits(cpu.get_spsr());
        }

        if result.is_some() {
            match cpu.status.cpsr.t() {
                ArmMode => cpu.pipeline_refill_arm(),
                ThumbMode => cpu.pipeline_refill_thumb(),
            }
        }
    }
}

pub fn read_status_mrs<const SPSR_DEST: bool>(cpu: &mut Arm7tdmi, opcode: u32) {
    let rd = (opcode >> 12) & 0xF; // destination register

    if SPSR_DEST {
        cpu.set_banked_register(rd, cpu.get_spsr());
    } else {
        cpu.set_banked_register(rd, cpu.status.cpsr.into_bits());
    }

    cpu.pipeline_state = access_code::SEQUENTIAL | access_code::CODE;
    cpu.registers.r15 += 4;
}

pub fn write_status_msr<const IMM: bool, const SPSR_DEST: bool>(cpu: &mut Arm7tdmi, opcode: u32) {
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
        cpu.get_banked_register(rm)
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

    cpu.pipeline_state = access_code::SEQUENTIAL | access_code::CODE;
    cpu.registers.r15 += 4;
}

pub fn multiply<const ACCUMULATE: bool, const SET_COND: bool>(cpu: &mut Arm7tdmi, opcode: u32) {
    cpu.pipeline_state = access_code::NONSEQUENTIAL | access_code::CODE;
    cpu.registers.r15 += 4;

    let rm = opcode & 0xF; // op1 reg value
    let rs = (opcode >> 8) & 0xF; // op 2 reg value
    let rn = (opcode >> 12) & 0xF; // accumulate reg value
    let rd = (opcode >> 16) & 0xF; // dest reg

    // multiply: rd = rm * rs;

    let op1 = cpu.get_banked_register(rm);
    let op2 = cpu.get_banked_register(rs);

    // todo handle extra i cycles
    let _i_cycles = 'block: {
        let add_cycle = if ACCUMULATE { 1 } else { 0 };

        match op2 & 0xFFFF_FF00 {
            0xFFFF_FF00 | 0x0000_0000 => break 'block 1 + add_cycle,
            _ => (),
        }

        match op2 & 0xFFFF_0000 {
            0xFFFF_0000 | 0x0000_0000 => break 'block 2 + add_cycle,
            _ => (),
        }

        match op2 & 0xFF00_0000 {
            0xFF00_0000 | 0x0000_0000 => break 'block 3 + add_cycle,
            _ => (),
        }

        4 + add_cycle
    };

    let mut result = op1.wrapping_mul(op2);

    if ACCUMULATE {
        let op3 = cpu.get_banked_register(rn);
        result = result.wrapping_add(op3);
    }

    if SET_COND {
        cpu.status.cpsr.set_n((result as i32).is_negative());
        cpu.status.cpsr.set_z(result == 0);
    }

    cpu.set_banked_register(rd, result);

    if rd == 15 {
        cpu.pipeline_refill_arm();
    }
}

pub fn multiply_long<const SIGNED: bool, const ACCUMULATE: bool, const SET_COND: bool>(
    cpu: &mut Arm7tdmi,
    opcode: u32,
) {
    cpu.pipeline_state = access_code::NONSEQUENTIAL | access_code::CODE;
    cpu.registers.r15 += 4;

    let rm = opcode & 0xF;
    let rs = (opcode >> 8) & 0xF;
    let rd_lo = (opcode >> 12) & 0xF;
    let rd_hi = (opcode >> 16) & 0xF;

    let op1: u32 = cpu.get_banked_register(rm);
    let op2: u32 = cpu.get_banked_register(rs);

    let _i_cycles = 'block: {
        let add_cycle = if ACCUMULATE { 1 } else { 0 };

        match (SIGNED, op2 & 0xFFFF_FF00) {
            (true, 0xFFFF_FF00 | 0) => break 'block 2 + add_cycle,
            (false, 0) => break 'block 2 + add_cycle,
            _ => (),
        }

        match (SIGNED, op2 & 0xFFFF_0000) {
            (true, 0xFFFF_0000 | 0) => break 'block 3 + add_cycle,
            (false, 0) => break 'block 3 + add_cycle,
            _ => (),
        }

        match (SIGNED, op2 & 0xFF00_0000) {
            (true, 0xFF00_0000 | 0) => break 'block 4 + add_cycle,
            (false, 0) => break 'block 4 + add_cycle,
            _ => (),
        }

        5 + add_cycle
    };

    let mut result = if SIGNED {
        ((op1 as i32 as i64) * (op2 as i32 as i64)) as u64
    } else {
        (op1 as u64) * (op2 as u64)
    };

    if ACCUMULATE {
        let op3: u64 = {
            let lo: u64 = cpu.get_banked_register(rd_lo).into();
            let hi: u64 = cpu.get_banked_register(rd_hi).into();
            (hi << 32) | lo
        };

        result = result.wrapping_add(op3);
    }

    if SET_COND {
        cpu.status.cpsr.set_n((result as i64).is_negative());
        cpu.status.cpsr.set_z(result == 0);
    }

    cpu.set_banked_register(rd_lo, result as u32);
    cpu.set_banked_register(rd_hi, (result >> 32) as u32);

    if rd_lo == 15 || rd_hi == 15 {
        cpu.pipeline_refill_arm();
    }
}

pub fn single_data_transfer<
    const IMM: bool,           // 0: offset is immediate value, 1: offset is a register
    const PRE_INDEX: bool,     // 0: post indexing, 1: pre indexing
    const INC: bool,           // 0: decrement, 1: increment
    const TRANSFER_BYTE: bool, // 0: transfer work size, 1: transfer byte size
    const WRITE_BACK: bool,    // 0: no write back, 1: write address to base
    const LOAD: bool,          // 0: store op, 1: load op
>(
    cpu: &mut Arm7tdmi,
    opcode: u32,
) {
    use super::common::arithmetic::*;

    let rd = (opcode >> 12) & 0xF; // destination/source register
    let rn = (opcode >> 16) & 0xF; // base register

    let mut offset = if IMM {
        opcode & 0xFFF
    } else {
        let shift_amount = (opcode >> 7) & 0x1F;
        let shift_type = (opcode >> 5) & 0x3;
        let value_to_shift = cpu.get_banked_register(opcode & 0xF);
        let is_immediate = true;

        match shift_type {
            0b00 => lsl(cpu, value_to_shift, shift_amount),
            0b01 => lsr(cpu, is_immediate, value_to_shift, shift_amount),
            0b10 => asr(cpu, is_immediate, value_to_shift, shift_amount),
            0b11 => ror(cpu, is_immediate, value_to_shift, shift_amount),
            _ => panic!("Invalid shift type! {shift_type}"),
        }
        .0
    };

    if !INC {
        offset = to_negative(offset); // convert to negative binary representation if subtracting with 2's complement
    }

    let address = if PRE_INDEX {
        cpu.get_banked_register(rn).wrapping_add(offset)
    } else {
        cpu.get_banked_register(rn)
    };

    cpu.pipeline_state = access_code::NONSEQUENTIAL | access_code::CODE;
    cpu.registers.r15 += 4;

    if LOAD {
        let load_value: u32 = if TRANSFER_BYTE {
            cpu.read_byte(address, access_code::NONSEQUENTIAL)
        } else {
            cpu.read_rotate_word(address, access_code::NONSEQUENTIAL)
        };

        // post index transfer will always do a writeback
        if WRITE_BACK || !PRE_INDEX {
            cpu.set_banked_register(rn, cpu.get_banked_register(rn).wrapping_add(offset));
        }

        // handle extra i cycle from load

        cpu.set_banked_register(rd, load_value);
    } else {
        let store_value = cpu.get_banked_register(rd);

        if TRANSFER_BYTE {
            cpu.write_byte(address, store_value as u8, access_code::NONSEQUENTIAL);
        } else {
            cpu.write_word(address, store_value, access_code::NONSEQUENTIAL);
        }

        // post index transfer will always do a writeback
        if WRITE_BACK || !PRE_INDEX {
            cpu.set_banked_register(rn, cpu.get_banked_register(rn).wrapping_add(offset));
        }
    }

    if (LOAD && rd == 15) || ((WRITE_BACK || !PRE_INDEX) && rn == 15) {
        cpu.pipeline_refill_arm();
    }
}

pub fn halfword_and_signed_data_transfer<
    const IMM: bool,
    const PRE_INDEX: bool,
    const INC: bool,
    const WRITE_BACK: bool,
    const LOAD: bool,
    const S: bool,
    const H: bool,
>(
    cpu: &mut Arm7tdmi,
    opcode: u32,
) {
    let rn = (opcode >> 16) & 0xF; // base register
    let rd = (opcode >> 12) & 0xF; // dest/source register
    let rm = opcode & 0xF; // offset register

    let offset = {
        let mut temp = if IMM {
            ((opcode >> 4) & 0xF0) | (opcode & 0xF)
        } else {
            cpu.get_banked_register(rm)
        };

        if !INC {
            temp = to_negative(temp);
        }

        temp
    };

    let address = if PRE_INDEX {
        cpu.get_banked_register(rn).wrapping_add(offset)
    } else {
        cpu.get_banked_register(rn)
    };

    cpu.pipeline_state = access_code::NONSEQUENTIAL | access_code::CODE;
    cpu.registers.r15 += 4;

    if LOAD {
        let load_value = match (S, H) {
            (true, true) => cpu.read_signed_halfword(address, access_code::NONSEQUENTIAL),
            (true, false) => cpu.read_signed_byte(address, access_code::NONSEQUENTIAL),
            (false, true) => cpu.read_rotate_halfword(address, access_code::NONSEQUENTIAL),
            (false, false) => panic!("Reserved for SWP instruction!"),
        };

        if WRITE_BACK || !PRE_INDEX {
            cpu.set_banked_register(rn, cpu.get_banked_register(rn).wrapping_add(offset));
        }

        // handle extra i cycle from load op
        cpu.bus.i_cycle();

        cpu.set_banked_register(rd, load_value);
    } else {
        let store_value = cpu.get_banked_register(rd);

        match (S, H) {
            (true, true) => panic!("Sign bit should not be set for store operation?"),
            (true, false) => panic!("Sign bit should not be set for store operation?"),
            (false, true) => {
                cpu.write_halfword(address, store_value as u16, access_code::NONSEQUENTIAL);
            }
            (false, false) => panic!("Reserved for SWP instruction!"),
        };

        if WRITE_BACK || !PRE_INDEX {
            cpu.set_banked_register(rn, cpu.get_banked_register(rn).wrapping_add(offset));
        }
    }

    if (LOAD && rd == 15) || ((WRITE_BACK || !PRE_INDEX) && rn == 15) {
        cpu.pipeline_refill_arm();
    }
}

#[derive(PartialEq)]
enum BlockTransferState {
    None,
    ForceUserMode,
    LoadPsr,
}

pub fn block_data_transfer<
    const PRE_INDEX: bool,
    const INC: bool,
    const S: bool, // load psr or force user mode
    const WRITE_BACK: bool,
    const LOAD: bool,
>(
    cpu: &mut Arm7tdmi,
    opcode: u32,
) {
    let rn = (opcode >> 16) & 0xF; // base address register
    let base = cpu.get_banked_register(rn);
    let mut rlist = opcode & 0xFFFF; // 16 bit register list for the 16 general purpose register

    // transfer begins at the lowest address first
    let base_address;
    let writeback_value;

    // handle empty register list which causes r15 to be loaded/stored
    if rlist == 0 {
        rlist = 0x8000;

        if INC {
            base_address = base.wrapping_add(0x40);
            writeback_value = base_address;
        } else {
            base_address = base.wrapping_sub(0x40);
            writeback_value = base_address;
        }
    } else {
        let transfer_byte_size = rlist.count_ones() * 4;

        if INC {
            base_address = base;
            writeback_value = base.wrapping_add(transfer_byte_size);
        } else {
            base_address = base.wrapping_sub(transfer_byte_size);
            writeback_value = base_address;
        }
    }

    let mode_backup = cpu.status.cpsr.mode_bits();
    let r15_in_transfer_list = rlist & (1 << 15) != 0;
    let block_transfer_state = if S {
        if LOAD && r15_in_transfer_list {
            BlockTransferState::LoadPsr
        } else {
            cpu.status.cpsr.set_mode_bits(Mode::User);
            BlockTransferState::ForceUserMode
        }
    } else {
        BlockTransferState::None
    };

    cpu.pipeline_state = access_code::NONSEQUENTIAL | access_code::CODE;
    cpu.registers.r15 += 4;

    let mut rlist_iter = (0..16)
        .filter(|i| rlist & (1 << i) != 0)
        .scan(0, |offset, i| {
            if (INC && PRE_INDEX) || (!INC && !PRE_INDEX) {
                *offset += 4;
                Some((base_address.wrapping_add(*offset), i))
            } else {
                let yield_value = Some((base_address.wrapping_add(*offset), i));
                *offset += 4;
                yield_value
            }
        });

    if let Some((address, register_id)) = rlist_iter.next() {
        let access = access_code::NONSEQUENTIAL;

        if LOAD {
            let load_value = cpu.read_word(address, access);

            if WRITE_BACK {
                cpu.set_banked_register(rn, writeback_value);
            }

            cpu.set_banked_register(register_id, load_value);
        } else {
            let store_value = cpu.get_banked_register(register_id);
            cpu.write_word(address, store_value, access);

            if WRITE_BACK {
                cpu.set_banked_register(rn, writeback_value);
            }
        }
    }

    for (address, register_id) in rlist_iter {
        let access = access_code::SEQUENTIAL;

        if LOAD {
            let load_value = cpu.read_word(address, access);
            cpu.set_banked_register(register_id, load_value);
        } else {
            let store_value = cpu.get_banked_register(register_id);
            cpu.write_word(address, store_value, access);
        }
    }

    match block_transfer_state {
        BlockTransferState::None => (),
        BlockTransferState::ForceUserMode => cpu.status.cpsr.set_mode_bits(mode_backup),
        BlockTransferState::LoadPsr => cpu.status.cpsr = StatusRegister::from_bits(cpu.get_spsr()),
    };

    if (LOAD && r15_in_transfer_list) || ((WRITE_BACK) && rn == 15) {
        match cpu.status.cpsr.t() {
            ArmMode => cpu.pipeline_refill_arm(),
            ThumbMode => cpu.pipeline_refill_thumb(),
        }
    }
}

pub fn data_swap<const SWAP_BYTE: bool>(cpu: &mut Arm7tdmi, opcode: u32) {
    let rm = opcode & 0xF; // source register
    let rd = (opcode >> 12) & 0xF; // destination register
    let rn = (opcode >> 16) & 0xF; // base register

    cpu.pipeline_state = access_code::NONSEQUENTIAL | access_code::CODE;
    cpu.registers.r15 += 4;

    // read from swap address
    let swap_address = cpu.get_banked_register(rn);
    let memory_value: u32 = if SWAP_BYTE {
        cpu.read_byte(swap_address, access_code::NONSEQUENTIAL)
    } else {
        cpu.read_rotate_word(swap_address, access_code::NONSEQUENTIAL)
    };

    // write rm register value into swap address
    let register_value = cpu.get_banked_register(rm);
    if SWAP_BYTE {
        cpu.write_byte(
            swap_address,
            register_value as u8,
            access_code::NONSEQUENTIAL | access_code::LOCK,
        );
    } else {
        cpu.write_word(
            swap_address,
            register_value,
            access_code::NONSEQUENTIAL | access_code::LOCK,
        );
    }

    cpu.set_banked_register(rd, memory_value);

    if rd == 15 {
        cpu.pipeline_refill_arm();
    }
}

pub fn software_interrupt(cpu: &mut Arm7tdmi, _opcode: u32) {
    cpu.registers.r14_svc = (cpu.registers.r15 - Wrapping(4)).0;

    cpu.registers.r15 = Wrapping(8);
    cpu.status.spsr_svc = cpu.status.cpsr;

    cpu.status.cpsr.set_i(true);
    cpu.status.cpsr.set_mode_bits(Mode::Supervisor);

    cpu.pipeline_refill_arm();
}

pub fn undefined_arm(_cpu: &mut Arm7tdmi, opcode: u32) {
    todo!("handle undefined opcode: {opcode}");
}

trait Negative<T> {
    fn negative(input: T) -> Self;
}

impl Negative<u32> for u32 {
    fn negative(input: u32) -> Self {
        (!input).wrapping_add(1)
    }
}

/// Retrieve raw binary representation of a negative number as a unsigned integer
fn to_negative<T: Negative<T>>(input: T) -> T {
    Negative::<T>::negative(input)
}
