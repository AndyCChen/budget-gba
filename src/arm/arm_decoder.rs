use crate::arm::core::Arm7tdmi;

pub fn branch_and_exchange(cpu: &mut Arm7tdmi, opcode: u32) {
    let branch_address = cpu.get_register_arm(opcode & 0xF);
    let is_thumb_mode = (branch_address & 0x1) == 1;
    cpu.status.cpsr.set_t(is_thumb_mode);

    cpu.registers.r15 = branch_address; // pc is updated so we need to refill instruction pipeline

    if is_thumb_mode {
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
    fn update_flags_logical(cpu: &mut Arm7tdmi, result: u32, shift_carry: bool) {
        cpu.status.cpsr.set_c(shift_carry);
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

    pub fn and<const SET_COND: bool>(
        cpu: &mut Arm7tdmi,
        op1: u32,
        op2: u32,
        shift_carry: bool,
    ) -> u32 {
        let result = op1 & op2;

        if SET_COND {
            update_flags_logical(cpu, result, shift_carry);
        }

        result
    }

    pub fn eor<const SET_COND: bool>(
        cpu: &mut Arm7tdmi,
        op1: u32,
        op2: u32,
        shift_carry: bool,
    ) -> u32 {
        let result = op1 ^ op2;

        if SET_COND {
            update_flags_logical(cpu, result, shift_carry);
        }

        result
    }

    pub fn sub<const SET_COND: bool>(cpu: &mut Arm7tdmi, op1: u32, op2: u32) -> u32 {
        let result = op1.wrapping_add(!op2 + 1);
        let carry = op2 <= op1;
        let overflow = ((result ^ op1) & (result ^ !op2) & 0x8000_0000) != 0;

        if SET_COND {
            update_flags_arithmetic(cpu, result, carry, overflow);
        }

        result
    }
}

pub fn data_processing<const IMM: bool, const OP_CODE: u8, const SET_COND: bool>(
    cpu: &mut Arm7tdmi,
    opcode: u32,
) {
    use self::data_op::*;

    let op1 = cpu.get_register_arm((opcode >> 16) & 0xF);
    let op2 = if IMM {
        let rotate = (opcode >> 8) & 0xF;
        let imm_u8 = opcode & 0xFF;

        imm_u8.rotate_right(rotate * 2)
    } else {
        todo!();
    };

    let shifter_carry = (op2 & 0x8000_0000) != 0;

    // 0b0000_0000_0000_0000_0000_0000_0000_0000

    let register_dest = (opcode >> 12) & 0xF;

    if register_dest == 0xF {
        let result = match OP_CODE {
            AND => and::<false>(cpu, op1, op2, shifter_carry),
            EOR => eor::<false>(cpu, op1, op2, shifter_carry),
            SUB => sub::<false>(cpu, op1, op2),
            RSB => sub::<false>(cpu, op2, op1),
            ADD => todo!("add"),
            ADC => todo!("adc"),
            SBC => todo!("sbc"),
            RSC => todo!("rsc"),
            TST => todo!("tst"),
            TEQ => todo!("teq"),
            CMP => todo!("cmp"),
            CMN => todo!("cmn"),
            ORR => todo!("orr"),
            MOV => todo!("mov"),
            BIC => todo!("bic"),
            MVN => todo!("mvn"),

            _ => panic!("Invalid data op! {OP_CODE}"),
        };

        if SET_COND {
            todo!("using pc as register dest not handled!");
        }

        cpu.set_register_arm(register_dest, result);
        cpu.pipeline_refill_arm(); // pc updated, so refill pipeline
    } else {
        let result = match OP_CODE {
            AND => and::<SET_COND>(cpu, op1, op2, shifter_carry),
            EOR => eor::<SET_COND>(cpu, op1, op2, shifter_carry),
            SUB => sub::<SET_COND>(cpu, op1, op2),
            RSB => sub::<SET_COND>(cpu, op2, op1),
            ADD => todo!("add"),
            ADC => todo!("adc"),
            SBC => todo!("sbc"),
            RSC => todo!("rsc"),
            TST => todo!("tst"),
            TEQ => todo!("teq"),
            CMP => todo!("cmp"),
            CMN => todo!("cmn"),
            ORR => todo!("orr"),
            MOV => todo!("mov"),
            BIC => todo!("bic"),
            MVN => todo!("mvn"),

            _ => panic!("Invalid data op! {OP_CODE}"),
        };

        cpu.set_register_arm(register_dest, result);
    };
}

pub fn undefined_arm(_cpu: &mut Arm7tdmi, opcode: u32) {
    panic!("undefined opcode! {opcode}");
}
