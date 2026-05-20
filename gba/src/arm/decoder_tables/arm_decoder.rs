pub struct ArmInstructionInfo {
    pub instruction: ArmInstruction,
    pub condition: u8,
}

#[rustfmt::skip]
pub enum ArmInstruction {
    // branch instructions
    Bx { rn: u8 },
    B { offset: u32 },
    Bl { offset: u32 },

    // data processing instructions
    And { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Eor { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Sub { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Rsb { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Add { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Adc { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Sbc { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Rsc { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Tst { rn: u8, op2: ArmDataOp2 },
    Teq { rn: u8, op2: ArmDataOp2 },
    Cmp { rn: u8, op2: ArmDataOp2 },
    Cmn { rn: u8, op2: ArmDataOp2 },
    Orr { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Mov { set_condition: bool, rd: u8, op2: ArmDataOp2 },
    Bic { set_condition: bool, rd: u8, rn: u8, op2: ArmDataOp2 },
    Mvn { set_condition: bool, rd: u8, op2: ArmDataOp2 },

    Mrs { spsr_dest: bool, rd: u8 },
    Msr { spsr_dest: bool, rm: u8 },
    MsrFlagOnly { source_operand: ShiftOperand, spsr_dest: bool },

    // multiply
    Mul { set_condition: bool, rd: u8, rm: u8, rs: u8 },
    Mla { set_condition: bool, rd: u8, rm: u8, rs: u8, rn: u8 },

    // multiply long
    Mull { set_condition: bool, is_signed: bool, rdlo: u8, rdhi: u8, rm: u8, rs: u8 },
    Mlal { set_condition: bool, is_signed: bool, rdlo: u8, rdhi: u8, rm: u8, rs: u8 },

    // ldr/str
    Ldr { is_byte: bool, rd: u8, address: LdrStrAddress },
    Str { is_byte: bool, rd: u8, address: LdrStrAddress },

    // ldr/str for halfword and byte
    Ldrh { rd: u8, address: LdrhStrhAddress },
    Strh { rd: u8, address: LdrhStrhAddress },
    Ldrsb { rd: u8, address: LdrhStrhAddress },
    Ldrsh { rd: u8, address: LdrhStrhAddress },
}

pub fn branch_and_exchange(opcode: u32) -> ArmInstructionInfo {
    let rn = (opcode & 0xF) as u8;
    ArmInstructionInfo {
        instruction: ArmInstruction::Bx { rn },
        condition: (opcode >> 28) as u8,
    }
}

pub fn branch_and_link(opcode: u32) -> ArmInstructionInfo {
    let link = (opcode >> 8) & 1 == 1;
    let mut offset = (opcode & 0xFFFFFF) << 2;

    if opcode & (1 << 23) != 0 {
        offset |= 0xFC00_0000;
    }

    let instruction = if link {
        ArmInstruction::Bl { offset }
    } else {
        ArmInstruction::B { offset }
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

// Enum to hold data processing operands that can vary.
// Operands are not fixed to a specific type,
// meaning they can be a register number, a shift value, a shiftname, etc...
#[derive(Clone, Copy)]
pub enum ArmDataOp2 {
    Expression(u32),
    Rm { rm: u8, shift: Shift },
}

#[derive(Clone, Copy)]
pub enum Shift {
    None,
    Lsl(ShiftOperand),
    Lsr(ShiftOperand),
    Asr(ShiftOperand),
    Ror(ShiftOperand),
    Rrx,
}

#[derive(Clone, Copy)]
pub enum ShiftOperand {
    Register(u8),
    Expression(u32),
}

pub fn data_processing(opcode: u32) -> ArmInstructionInfo {
    use crate::arm::arm_data_op::*;
    use crate::arm::opcode_tables::{ASR, LSL, LSR, ROR};

    let is_op2_immediate = (opcode >> 25) & 0x1 == 1;
    let data_op = ((opcode >> 21) & 0xF) as u8;
    let set_condition = (opcode >> 20) & 0x1 == 1;
    let rn = ((opcode >> 16) & 0xF) as u8;
    let rd = ((opcode >> 12) & 0xF) as u8;

    let op2 = if is_op2_immediate {
        let shift_amount = ((opcode >> 8) & 0xF) * 2;
        let value_to_shift = opcode & 0xFF;
        ArmDataOp2::Expression(value_to_shift.rotate_right(shift_amount))
    } else {
        let register_specified_shift = (opcode >> 4 & 1) == 1;
        let shift_type = (opcode >> 5) & 3;
        let rm = (opcode & 0xF) as u8;
        let mut is_zero_shift = false;

        let shift_operand = if register_specified_shift {
            ShiftOperand::Register(((opcode >> 8) & 0xF) as u8)
        } else {
            let mut shift_amount = (opcode >> 7) & 0x1F;
            is_zero_shift = shift_amount == 0;

            // LSR #0 and ASR #0 encodes #32 bit shifts
            if is_zero_shift && matches!(shift_type as u8, LSR | ASR) {
                shift_amount = 32;
            }

            ShiftOperand::Expression(shift_amount)
        };

        #[rustfmt::skip]
        let shift = match shift_type as u8 {
            LSL => if !register_specified_shift && is_zero_shift { Shift::None } else { Shift::Lsl(shift_operand) },
            LSR => Shift::Lsr(shift_operand),
            ASR => Shift::Asr(shift_operand),
            ROR => if !register_specified_shift && is_zero_shift { Shift::Rrx } else { Shift::Ror(shift_operand) },
            _ => panic!("Invalid shift type {shift_type}"),
        };

        ArmDataOp2::Rm { rm, shift }
    };

    #[rustfmt::skip]
    let instruction = match data_op {
        AND => ArmInstruction::And { set_condition, rd, rn, op2 },
        EOR => ArmInstruction::Eor { set_condition, rd, rn, op2 },
        SUB => ArmInstruction::Sub { set_condition, rd, rn, op2 },
        RSB => ArmInstruction::Rsb { set_condition, rd, rn, op2 },
        ADD => ArmInstruction::Add { set_condition, rd, rn, op2 },
        ADC => ArmInstruction::Adc { set_condition, rd, rn, op2 },
        SBC => ArmInstruction::Sbc { set_condition, rd, rn, op2 },
        RSC => ArmInstruction::Rsc { set_condition, rd, rn, op2 },
        TST => ArmInstruction::Tst { rn, op2 },
        TEQ => ArmInstruction::Teq { rn, op2 },
        CMP => ArmInstruction::Cmp { rn, op2 },
        CMN => ArmInstruction::Cmn { rn, op2 },
        ORR => ArmInstruction::Orr { set_condition, rd, rn, op2 }, 
        MOV => ArmInstruction::Mov { set_condition, rd, op2 },
        BIC => ArmInstruction::Bic { set_condition, rd, rn, op2 }, 
        MVN => ArmInstruction::Mvn { set_condition, rd, op2 },
        _ => panic!("Invalid data op! {data_op}"),
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

pub fn read_status_mrs(opcode: u32) -> ArmInstructionInfo {
    let spsr_dest = (opcode >> 22) & 1 == 1;
    let rd = (opcode >> 12) as u8;

    let instruction = ArmInstruction::Mrs { spsr_dest, rd };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

pub fn write_status_msr(opcode: u32) -> ArmInstructionInfo {
    let is_immediate = (opcode >> 25) & 1 == 1;
    let spsr_dest = (opcode >> 22) & 1 == 1;
    let set_flag_only = (opcode >> 16) & 0xF == 0b1000;
    let rm = (opcode & 0xF) as u8;

    let instruction = if set_flag_only {
        let source_operand = if is_immediate {
            let rotate = ((opcode >> 8) & 0xF) * 2;
            let value_to_rotate = opcode & 0xFF;
            ShiftOperand::Expression(value_to_rotate.rotate_right(rotate))
        } else {
            ShiftOperand::Register(rm)
        };

        ArmInstruction::MsrFlagOnly {
            source_operand,
            spsr_dest,
        }
    } else {
        ArmInstruction::Msr { spsr_dest, rm }
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

pub fn multiply(opcode: u32) -> ArmInstructionInfo {
    let is_accumulate = (opcode >> 21) == 1;
    let set_condition = (opcode >> 20) == 1;
    let rd = ((opcode >> 16) & 0xF) as u8;
    let rn = ((opcode >> 12) & 0xF) as u8;
    let rs = ((opcode >> 8) & 0xF) as u8;
    let rm = (opcode & 0xF) as u8;

    #[rustfmt::skip]
    let instruction = if is_accumulate {
        ArmInstruction::Mla { set_condition, rd, rm, rs, rn }
    } else {
        ArmInstruction::Mul { set_condition, rd, rm, rs }
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

pub fn multiply_long(opcode: u32) -> ArmInstructionInfo {
    let is_signed = (opcode >> 22) == 1;
    let is_accumulate = (opcode >> 21) == 1;
    let set_condition = (opcode >> 20) == 1;

    let rdhi = ((opcode >> 16) & 0xF) as u8;
    let rdlo = ((opcode >> 12) & 0xF) as u8;
    let rs = ((opcode >> 8) & 0xF) as u8;
    let rm = (opcode & 0xF) as u8;

    #[rustfmt::skip]
    let instruction = if is_accumulate {
        ArmInstruction::Mlal { set_condition, is_signed, rdlo, rdhi, rm, rs }
    } else {
        ArmInstruction::Mull { set_condition, is_signed, rdlo, rdhi, rm, rs }
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy)]
pub enum LdrStrAddress {
    PcRelative(u32),

    PreIndexZero  { rn: u8 },
    PostIndexZero { rn: u8 },

    PreIndexExpression  { rn: u8, is_increment: bool, expr: u32, is_write_back: bool },
    PostIndexExpression { rn: u8, is_increment: bool, expr: u32 },

    PreIndexShifted     { rn: u8, is_increment: bool, rm: u8, shift: LdrStrAddressShift, is_write_back: bool },
    PostIndexShifted    { rn: u8, is_increment: bool, rm: u8, shift: LdrStrAddressShift },
}

#[derive(Clone, Copy)]
pub enum LdrStrAddressShift {
    None,
    Lsl(u32),
    Lsr(u32),
    Asr(u32),
    Ror(u32),
    Rrx,
}

pub fn single_data_transfer(opcode: u32) -> ArmInstructionInfo {
    use crate::arm::opcode_tables::to_negative_u32;
    use crate::arm::opcode_tables::{ASR, LSL, LSR, ROR};

    let is_immediate = (opcode >> 25) & 1 == 0;
    let is_pre_index = (opcode >> 24) & 1 == 1;
    let is_increment = (opcode >> 23) & 1 == 1;
    let is_byte = (opcode >> 22) & 1 == 1;
    let is_write_back = (opcode >> 21) & 1 == 1;
    let is_load = (opcode >> 20) & 1 == 1;
    let rn = ((opcode >> 16) & 0xF) as u8;
    let rd = ((opcode >> 12) & 0xF) as u8;

    let address = if is_immediate && rn == 0xF {
        let mut offset = opcode & 0xFFF;
        if !is_increment {
            offset = to_negative_u32(offset)
        }

        LdrStrAddress::PcRelative(offset)
    } else if is_immediate {
        let expr = opcode & 0xFFF;
        let is_zero = expr == 0;

        #[rustfmt::skip]
        let adr = match (is_zero, is_pre_index) {
            (true, true) =>   LdrStrAddress::PreIndexZero        { rn },
            (true, false) =>  LdrStrAddress::PostIndexZero       { rn },
            (false, true) =>  LdrStrAddress::PreIndexExpression  { rn, is_increment, expr, is_write_back },
            (false, false) => LdrStrAddress::PostIndexExpression { rn, is_increment, expr },
        };

        adr
    } else {
        let mut shift_amount = (opcode >> 7) & 0x1F;
        let shift_type = (opcode >> 5) & 0b11;
        let rm = (opcode & 0xF) as u8;

        let is_zero_shift = shift_amount == 0;

        // LSR #0 and ASR #0 encodes #32 bit shifts
        if is_zero_shift && matches!(shift_type as u8, LSR | ASR) {
            shift_amount = 32;
        }

        #[rustfmt::skip]
        let shift = match shift_type as u8 {
            LSL => if is_zero_shift { LdrStrAddressShift::None } else { LdrStrAddressShift::Lsl(shift_amount) },
            LSR => LdrStrAddressShift::Lsr(shift_amount),
            ASR => LdrStrAddressShift::Asr(shift_amount),
            ROR => if is_zero_shift { LdrStrAddressShift::Rrx } else { LdrStrAddressShift::Ror(shift_amount) },
            _ => panic!("Invalid shift type {shift_type}"),
        };

        #[rustfmt::skip]
        let adr = match is_pre_index {
            true => LdrStrAddress::PreIndexShifted   { rn, is_increment, rm, shift, is_write_back },
            false => LdrStrAddress::PostIndexShifted { rn, is_increment, rm, shift },
        };

        adr
    };

    #[rustfmt::skip]
    let instruction = match is_load {
        true => ArmInstruction::Ldr { is_byte, rd, address },
        false => ArmInstruction::Str { is_byte, rd, address },
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy)]
pub enum LdrhStrhAddress {
    PcRelative(u32),

    PreIndexZero  { rn: u8 },
    PostIndexZero { rn: u8 },

    PreIndexExpression  { rn: u8, is_increment: bool, expr: u32, is_write_back: bool },
    PostIndexExpression { rn: u8, is_increment: bool, expr: u32 },

    PreIndexRegister     { rn: u8, is_increment: bool, rm: u8, is_write_back: bool },
    PostIndexRegister    { rn: u8, is_increment: bool, rm: u8},
}

pub fn halfword_and_signed_data_transfer(opcode: u32) -> ArmInstructionInfo {
    use crate::arm::opcode_tables::to_negative_u32;

    let is_pre_index = (opcode >> 24) & 1 == 1;
    let is_increment = (opcode >> 23) & 1 == 1;
    let is_immediate = (opcode >> 22) & 1 == 1;
    let is_write_back = (opcode >> 21) & 1 == 1;
    let is_load = (opcode >> 20) & 1 == 1;

    let rn = ((opcode >> 16) & 0xF) as u8;
    let rd = ((opcode >> 12) & 0xF) as u8;
    let is_signed = (opcode >> 6) & 1 == 1;
    let is_halfword = (opcode >> 5) & 1 == 1;

    let address = if is_immediate && rn == 0xF {
        let mut offset = ((opcode >> 4) & 0xF) | (opcode & 0xF);
        if !is_increment {
            offset = to_negative_u32(offset)
        }

        LdrhStrhAddress::PcRelative(offset)
    } else if is_immediate {
        let expr = ((opcode >> 4) & 0xF) | (opcode & 0xF);
        let is_zero = expr == 0;

        #[rustfmt::skip]
        let adr = match (is_zero, is_pre_index) {
            (true, true) => LdrhStrhAddress::PreIndexZero   { rn },
            (true, false) => LdrhStrhAddress::PostIndexZero { rn },

            (false, true) => LdrhStrhAddress::PreIndexExpression   { rn, is_increment, expr, is_write_back },
            (false, false) => LdrhStrhAddress::PostIndexExpression { rn, is_increment, expr },
        };

        adr
    } else {
        let rm = (opcode & 0xF) as u8;

        #[rustfmt::skip]
        let adr = match is_pre_index {
            true => LdrhStrhAddress::PreIndexRegister   { rn, is_increment, rm, is_write_back },
            false => LdrhStrhAddress::PostIndexRegister { rn, is_increment, rm },
        };

        adr
    };

    let instruction = match (is_load, is_signed, is_halfword) {
        // load
        (true, true, true) => ArmInstruction::Ldrsh { rd, address },
        (true, true, false) => ArmInstruction::Ldrsb { rd, address },
        (true, false, true) => ArmInstruction::Ldrh { rd, address },
        (true, false, false) => panic!("Reserved for swp!"),

        // store
        (false, true, true) => panic!("Store signed halfword not allowwed!"),
        (false, true, false) => panic!("Store signed byte not allowed!"),
        (false, false, true) => ArmInstruction::Strh { rd, address },
        (false, false, false) => panic!("Reserved for swp!"),
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

pub fn undefined_arm(_opcode: u32) -> ArmInstructionInfo {
    todo!("Handle undefined instruction?")
}
