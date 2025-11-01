use std::num::Wrapping;

use super::common::arithmetic::*;
use crate::arm::{
    constants::access_code,
    core::{Arm7tdmi, Mode},
    opcode_tables::common::reg_constant::*,
};

pub fn move_shifted<const SHIFT_OP: u8>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rd: u32 = (opcode & 0x7).into();
    let rs: u32 = ((opcode >> 3) & 0x7).into();
    let shift_amount = (opcode >> 6) & 0x1F;

    let (result, carry_from_shift) = {
        let value = cpu.get_banked_register(rs);

        match SHIFT_OP {
            LSL => lsl(cpu, value, shift_amount.into()),
            LSR => lsr(cpu, true, value, shift_amount.into()),
            ASR => asr(cpu, true, value, shift_amount.into()),
            _ => panic!("Invalid shift op!"),
        }
    };

    let result = mov::<true>(cpu, result, carry_from_shift);
    cpu.set_banked_register(rd, result);
}

pub fn add_subtract<const IMM: bool, const IS_SUBTRACT: bool>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rd: u32 = (opcode & 0x7).into(); // destination register
    let rs: u32 = ((opcode >> 3) & 0x7).into(); // source register
    let rn: u32 = ((opcode >> 6) & 0x7).into(); // 3 bit immediate value or register id

    let op1 = cpu.get_banked_register(rs);
    let op2: u32 = if IMM { rn } else { cpu.get_banked_register(rn) };

    let result = if IS_SUBTRACT {
        sub::<true>(cpu, op1, op2)
    } else {
        add::<true>(cpu, op1, op2)
    };

    cpu.set_banked_register(rd, result);
}

pub fn mov_cmp_add_sub_immediate<const OP: u8>(cpu: &mut Arm7tdmi, opcode: u16) {
    const MOV: u8 = 0;
    const CMP: u8 = 1;
    const ADD: u8 = 2;
    const SUB: u8 = 3;

    cpu.registers.r15 += 2;

    let rd: u32 = ((opcode >> 8) & 0x7).into();
    let immediate_value: u32 = (opcode & 0xFF).into();

    let op1 = cpu.get_banked_register(rd);

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
        cpu.set_banked_register(rd, value);
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

    let rd = (opcode & 0x7).into();
    let rs = ((opcode >> 3) & 0x7).into();

    let op1 = cpu.get_banked_register(rd);
    let op2 = cpu.get_banked_register(rs);

    if matches!(OP, alu_op::LSL | alu_op::LSR | alu_op::ASR | alu_op::ROR) {
        // handle extra i cycle from register specified shift
        cpu.bus.i_cycle();
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
        cpu.set_banked_register(rd, value);
    }
}

pub fn add_cmp_mov_hi<const OP: u8, const H1: bool, const H2: bool>(
    cpu: &mut Arm7tdmi,
    opcode: u16,
) {
    const ADD: u8 = 0;
    const CMP: u8 = 1;
    const MOV: u8 = 2;
    const BX: u8 = 3;

    let rd: u32 = (u32::from(H1) << 3) | Into::<u32>::into(opcode & 0x7);
    let rs: u32 = (u32::from(H2) << 3) | Into::<u32>::into((opcode >> 3) & 0x7);

    let op1 = cpu.get_banked_register(rd);
    let op2 = if rs == 15 {
        cpu.get_banked_register(rs) & !1
    } else {
        cpu.get_banked_register(rs)
    };

    cpu.registers.r15 += 2;

    match OP {
        ADD | MOV => {
            let result = match OP {
                ADD => add::<false>(cpu, op1, op2),
                MOV => mov::<false>(cpu, op2, cpu.status.cpsr.c()),
                _ => panic!(),
            };

            cpu.set_banked_register(rd, result);

            if rd == 15 {
                cpu.registers.r15.0 &= !1;
                cpu.pipeline_refill_thumb();
            }
        }
        CMP => {
            sub::<true>(cpu, op1, op2);
        }
        BX => {
            // thumb mode
            if op2 & 1 == 1 {
                cpu.status.cpsr.set_t(true);
                cpu.registers.r15.0 = op2 & !1;
                cpu.pipeline_refill_thumb();
            }
            // arm mode
            else {
                cpu.status.cpsr.set_t(false);
                cpu.registers.r15.0 = op2;
                cpu.pipeline_refill_arm();
            }
        }

        _ => panic!("Invalid OP! {OP}"),
    };
}

pub fn pc_relative_load(cpu: &mut Arm7tdmi, opcode: u16) {
    let rd: u32 = ((opcode >> 8) & 0x7).into();
    let offset: u32 = ((opcode & 0xFF) * 4).into();

    let address = (cpu.registers.r15.0 & !2).wrapping_add(offset);
    let value = cpu.read_word(address, access_code::NONSEQUENTIAL);
    cpu.set_banked_register(rd, value);

    // todo handle i cycle
    cpu.bus.i_cycle();

    cpu.registers.r15 += 2;
}

pub fn load_store_register_offset<const LOAD: bool, const TRANSFER_BYTE: bool>(
    cpu: &mut Arm7tdmi,
    opcode: u16,
) {
    cpu.registers.r15 += 2;

    let rd: u32 = (opcode & 0x7).into(); // source/dest register
    let rb: u32 = ((opcode >> 3) & 0x7).into(); // base register
    let ro: u32 = ((opcode >> 6) & 0x7).into(); // offset register

    let address = cpu
        .get_banked_register(rb)
        .wrapping_add(cpu.get_banked_register(ro));

    if LOAD {
        let load_value = if TRANSFER_BYTE {
            cpu.read_byte(address, access_code::NONSEQUENTIAL)
        } else {
            cpu.read_rotate_word(address, access_code::NONSEQUENTIAL)
        };

        // todo handle i cycle for load op
        cpu.bus.i_cycle();

        cpu.set_banked_register(rd, load_value);
    } else {
        let store_value = cpu.get_banked_register(rd);

        if TRANSFER_BYTE {
            cpu.write_byte(address, store_value as u8, access_code::NONSEQUENTIAL);
        } else {
            cpu.write_word(address, store_value, access_code::NONSEQUENTIAL);
        }
    }
}

pub fn load_store_sign_extended<const OP: u8>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rd = u32::from(opcode) & 0x7; // destination
    let rb = (u32::from(opcode) >> 3) & 0x7; // base
    let ro = (u32::from(opcode) >> 6) & 0x7; // offset

    let address = cpu
        .get_banked_register(rb)
        .wrapping_add(cpu.get_banked_register(ro));

    match OP {
        // store halfword
        0 => {
            let store_value = cpu.get_banked_register(rd);
            cpu.write_halfword(address, store_value as u16, access_code::NONSEQUENTIAL);
        }
        // load sign extended byte
        1 => {
            let load_value = cpu.read_signed_byte(address, access_code::NONSEQUENTIAL);
            cpu.set_banked_register(rd, load_value);
        }
        // load halfword
        2 => {
            let load_value = cpu.read_rotate_halfword(address, access_code::NONSEQUENTIAL);
            cpu.set_banked_register(rd, load_value);
        }
        // load sign extended halfword
        3 => {
            let load_value = cpu.read_signed_halfword(address, access_code::NONSEQUENTIAL);
            cpu.set_banked_register(rd, load_value);
        }
        _ => panic!("Invalid OP! {OP}"),
    }
}

pub fn load_store_immediate_offset<const TRANSFER_BYTE: bool, const LOAD: bool>(
    cpu: &mut Arm7tdmi,
    opcode: u16,
) {
    cpu.registers.r15 += 2;

    let rd = Into::<u32>::into(opcode & 7); // src/dest register
    let rb = Into::<u32>::into((opcode >> 3) & 7); // base address register
    let offset = Into::<u32>::into((opcode >> 6) & 0x1F) << if TRANSFER_BYTE { 0 } else { 2 };

    let address = cpu.get_banked_register(rb).wrapping_add(offset);

    if LOAD {
        let load_value = if TRANSFER_BYTE {
            cpu.read_byte(address, access_code::NONSEQUENTIAL)
        } else {
            cpu.read_rotate_word(address, access_code::NONSEQUENTIAL)
        };

        // todo handle i cycle for load op
        cpu.bus.i_cycle();

        cpu.set_banked_register(rd, load_value);
    } else {
        let store_value = cpu.get_banked_register(rd);

        if TRANSFER_BYTE {
            cpu.write_byte(address, store_value as u8, access_code::NONSEQUENTIAL);
        } else {
            cpu.write_word(address, store_value, access_code::NONSEQUENTIAL);
        }
    }
}

pub fn load_store_halfword_immediate_offset<const LOAD: bool>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rd = Into::<u32>::into(opcode & 7); // src/dest register
    let rb = Into::<u32>::into((opcode >> 3) & 7); // base register
    let offset = Into::<u32>::into((opcode >> 6) & 0x1F) << 1;

    let address = cpu.get_banked_register(rb).wrapping_add(offset);

    if LOAD {
        let load_value = cpu.read_rotate_halfword(address, access_code::NONSEQUENTIAL);

        // todo handle i cycle from load op
        cpu.bus.i_cycle();

        cpu.set_banked_register(rd, load_value);
    } else {
        let store_value = cpu.get_banked_register(rd);
        cpu.write_halfword(address, store_value as u16, access_code::NONSEQUENTIAL);
    }
}

pub fn sp_load_store_relative_offset<const LOAD: bool>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rd: u32 = ((opcode >> 8) & 7).into();
    let offset: u32 = ((opcode & 0xFF) << 2).into();

    let address = cpu.get_banked_register(STACK_POINTER).wrapping_add(offset);

    if LOAD {
        let load_value = cpu.read_rotate_word(address, access_code::NONSEQUENTIAL);

        // todo handle i cycle
        cpu.bus.i_cycle();

        cpu.set_banked_register(rd, load_value);
    } else {
        let store_value = cpu.get_banked_register(rd);
        cpu.write_word(address, store_value, access_code::NONSEQUENTIAL);
    }
}

pub fn pc_sp_load_address<const SP: bool>(cpu: &mut Arm7tdmi, opcode: u16) {
    let rd = u32::from((opcode >> 8) & 7);
    let offset = u32::from((opcode & 0xFF) << 2);

    let address = if SP {
        cpu.get_banked_register(STACK_POINTER).wrapping_add(offset)
    } else {
        let pc_value = cpu.registers.r15.0 & !2;
        pc_value.wrapping_add(offset)
    };

    cpu.set_banked_register(rd, address);

    cpu.registers.r15 += 2;
}

pub fn add_sub_sp<const NEGATIVE_OFFSET: bool>(cpu: &mut Arm7tdmi, opcode: u16) {
    let offset = u32::from((opcode & 0x7F) << 2);

    let result = if NEGATIVE_OFFSET {
        cpu.get_banked_register(STACK_POINTER).wrapping_sub(offset)
    } else {
        cpu.get_banked_register(STACK_POINTER).wrapping_add(offset)
    };

    cpu.set_banked_register(STACK_POINTER, result);

    cpu.registers.r15 += 2;
}

pub fn push_pop_register<
    const LOAD: bool,
    const PC_LR_BIT: bool, // 0: Leave LR/PC alone, 1: store LR or load PC
>(
    cpu: &mut Arm7tdmi,
    opcode: u16,
) {
    cpu.registers.r15 += 2;

    let rlist = opcode & 0xFF;
    let base = cpu.get_banked_register(STACK_POINTER);
    let base_address;

    if !PC_LR_BIT && rlist == 0 {
        let access = access_code::NONSEQUENTIAL;

        if LOAD {
            let pc_load_value = cpu.read_word(base, access);
            cpu.set_banked_register(PROGRAM_COUNTER, pc_load_value);
            cpu.set_banked_register(STACK_POINTER, base.wrapping_add(0x40));

            cpu.pipeline_refill_thumb();
        } else {
            base_address = base.wrapping_sub(0x40);
            cpu.set_banked_register(STACK_POINTER, base_address);
            cpu.write_word(base_address, cpu.registers.r15.0, access);
        }

        return;
    }

    let transfer_byte_size = ((opcode & 0xFF).count_ones() + u32::from(PC_LR_BIT)) * 4;

    // update stack pointer
    if LOAD {
        base_address = base;
        cpu.set_banked_register(STACK_POINTER, base.wrapping_add(transfer_byte_size));
    } else {
        base_address = base.wrapping_sub(transfer_byte_size);
        cpu.set_banked_register(STACK_POINTER, base_address);
    }

    let rlist_iter = (0..8)
        .filter(|i| rlist & (1 << i) != 0)
        .chain(
            if LOAD {
                PROGRAM_COUNTER..=PROGRAM_COUNTER
            } else {
                LINK_REGISTER..=LINK_REGISTER
            }
            .filter(|_| PC_LR_BIT),
        )
        .scan(0, |offset, i| {
            let yield_value = Some((base_address.wrapping_add(*offset), i));
            *offset += 4;
            yield_value
        });

    let mut access = access_code::NONSEQUENTIAL;
    for (address, register_id) in rlist_iter {
        if LOAD {
            let pop_value = cpu.read_word(address, access);
            cpu.set_banked_register(register_id, pop_value);
        } else {
            let push_value = cpu.get_banked_register(register_id);
            cpu.write_word(address, push_value, access);
        }

        access = access_code::SEQUENTIAL;
    }

    if LOAD {
        // handle extra i cycle from load op
        cpu.bus.i_cycle();
    }

    if PC_LR_BIT && LOAD {
        cpu.registers.r15.0 &= !1;
        cpu.pipeline_refill_thumb();
    }
}

pub fn multiple_load_store<const LOAD: bool>(cpu: &mut Arm7tdmi, opcode: u16) {
    cpu.registers.r15 += 2;

    let rlist = opcode & 0xFF;
    let rb: u32 = ((opcode >> 8) & 0x7).into(); // base register

    let base = cpu.get_banked_register(rb);

    // handle empty rlist
    if rlist == 0 {
        let access = access_code::NONSEQUENTIAL;

        if LOAD {
            let load_value = cpu.read_word(base, access);
            cpu.set_banked_register(PROGRAM_COUNTER, load_value);
            cpu.pipeline_refill_thumb();
        } else {
            cpu.write_word(base, cpu.registers.r15.0, access);
        }

        cpu.set_banked_register(rb, base.wrapping_add(0x40));

        return;
    }

    let mut rlist_iter = (0..8)
        .filter(|i| rlist & (1 << i) != 0)
        .scan(0, |offset, i| {
            let yield_value = Some((base.wrapping_add(*offset), i));
            *offset += 4;
            yield_value
        });

    if let Some((address, register_id)) = rlist_iter.next() {
        let access = access_code::NONSEQUENTIAL;
        let transfer_byte_size = rlist.count_ones() * 4;
        let write_back = base.wrapping_add(transfer_byte_size);

        if LOAD {
            let load_value = cpu.read_word(address, access);
            cpu.set_banked_register(rb, write_back);
            cpu.set_banked_register(register_id, load_value);
        } else {
            let store_value = cpu.get_banked_register(register_id);
            cpu.write_word(address, store_value, access);
            cpu.set_banked_register(rb, write_back);
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

    // todo handle i cycle from load op
    if LOAD {
        cpu.bus.i_cycle();
    }
}

pub fn conditional_branch<const COND: u8>(cpu: &mut Arm7tdmi, opcode: u16) {
    use crate::arm::constants::arm_condition_code::*;

    let do_branch = match COND {
        EQ => cpu.status.cpsr.z(),
        NE => !cpu.status.cpsr.z(),
        CS => cpu.status.cpsr.c(),
        CC => !cpu.status.cpsr.c(),
        MI => cpu.status.cpsr.n(),
        PL => !cpu.status.cpsr.n(),
        VS => cpu.status.cpsr.v(),
        VC => !cpu.status.cpsr.v(),
        HI => cpu.status.cpsr.c() && !cpu.status.cpsr.z(),
        LS => !cpu.status.cpsr.c() || cpu.status.cpsr.z(),
        GE => cpu.status.cpsr.n() == cpu.status.cpsr.v(),
        LT => cpu.status.cpsr.n() != cpu.status.cpsr.v(),
        GT => !cpu.status.cpsr.z() && (cpu.status.cpsr.n() == cpu.status.cpsr.v()),
        LE => cpu.status.cpsr.z() || (cpu.status.cpsr.n() != cpu.status.cpsr.v()),
        14 => panic!("Undefined cond 13!"),
        15 => panic!("Condition 15 defines SWI instruction"),
        _ => panic!("Invalid cond: {COND}"),
    };

    if !do_branch {
        cpu.registers.r15 += 2;
        return;
    }

    let mut branch_offset: u32 = ((opcode & 0xFF) << 1).into();

    // positive
    if branch_offset & 0x100 == 0 {
        cpu.registers.r15 += branch_offset;
    } else {
        branch_offset |= 0xFFFF_FF00;
        branch_offset = !branch_offset + 1;
        cpu.registers.r15 -= branch_offset;
    };

    cpu.pipeline_refill_thumb();
}

pub fn software_interrupt(cpu: &mut Arm7tdmi, _opcode: u16) {
    cpu.registers.r14_svc = cpu.registers.r15.0.wrapping_sub(2);

    cpu.registers.r15 = Wrapping(8);
    cpu.status.spsr_svc = cpu.status.cpsr;

    cpu.status.cpsr.set_i(true);
    cpu.status.cpsr.set_mode_bits(Mode::Supervisor);
    cpu.status.cpsr.set_t(false); // switch to arm mode

    cpu.pipeline_refill_arm();
}

pub fn unconditional_branch(cpu: &mut Arm7tdmi, opcode: u16) {
    let mut branch_offset: u32 = ((opcode & 0x7FF) << 1).into();

    // positive
    if branch_offset & 0x800 == 0 {
        cpu.registers.r15 += branch_offset;
    } else {
        branch_offset |= 0xFFFF_F800;
        branch_offset = !branch_offset + 1;
        cpu.registers.r15 -= branch_offset;
    }

    cpu.pipeline_refill_thumb();
}

pub fn long_branch_with_link<
    const H_BIT: bool, // 0: offset high, 1: offset low
>(
    cpu: &mut Arm7tdmi,
    opcode: u16,
) {
    if H_BIT {
        let offset_lo: u32 = ((opcode & 0x7FF) << 1).into();

        let branch_address = cpu
            .get_banked_register(LINK_REGISTER)
            .wrapping_add(offset_lo);

        cpu.set_banked_register(LINK_REGISTER, cpu.registers.r15.0.wrapping_sub(2) | 1);

        cpu.registers.r15.0 = branch_address & !1;
        cpu.pipeline_refill_thumb();
    } else {
        let mut offset_hi = (u32::from(opcode) & 0x7FF) << 12;

        // positive
        let lr_hi = if offset_hi & (1 << 22) == 0 {
            cpu.registers.r15.0.wrapping_add(offset_hi)
        } else {
            offset_hi |= 0xFFC0_0000;
            offset_hi = !offset_hi + 1;
            cpu.registers.r15.0.wrapping_sub(offset_hi)
        };

        cpu.set_banked_register(LINK_REGISTER, lr_hi);
        cpu.registers.r15 += 2;
    }
}

pub fn undefined_thumb(_cpu: &mut Arm7tdmi, opcode: u16) {
    todo!("handle undefined opcode: {opcode}");
}
