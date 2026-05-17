pub struct ArmInstructionInfo {
    pub instruction: ArmInstruction,
    pub condition: u8,
}

pub enum ArmInstruction {
    // branch instructions
    Bx {
        rn: u8,
    },
    B {
        offset: u32,
    },
    Bl {
        offset: u32,
    },

    // data processing instructions
    And {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Eor {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Sub {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Rsb {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Add {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Adc {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Sbc {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Rsc {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Tst {
        rn: u8,
        op2: ArmDataOp2,
    },
    Teq {
        rn: u8,
        op2: ArmDataOp2,
    },
    Cmp {
        rn: u8,
        op2: ArmDataOp2,
    },
    Cmn {
        rn: u8,
        op2: ArmDataOp2,
    },
    Orr {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Mov {
        set_condition: bool,
        rd: u8,
        op2: ArmDataOp2,
    },
    Bic {
        set_condition: bool,
        rd: u8,
        rn: u8,
        op2: ArmDataOp2,
    },
    Mvn {
        set_condition: bool,
        rd: u8,
        op2: ArmDataOp2,
    },
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
    Rm(u8, Shift),
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
            let mut expr = (opcode >> 7) & 0x1F;
            is_zero_shift = expr == 0;

            // LSR #0 and ASR #0 encodes #32 bit shifts
            if is_zero_shift && matches!(shift_type as u8, LSR | ASR) {
                expr = 32;
            }

            ShiftOperand::Expression(expr)
        };

        #[rustfmt::skip]
        let operand2 = match shift_type as u8 {
            LSL => ArmDataOp2::Rm(rm, if !register_specified_shift && is_zero_shift { Shift::None } else {Shift::Lsl(shift_operand) }),
            LSR => ArmDataOp2::Rm(rm, Shift::Lsr(shift_operand) ),
            ASR => ArmDataOp2::Rm(rm, Shift::Asr(shift_operand) ),
            ROR => ArmDataOp2::Rm(rm, if !register_specified_shift && is_zero_shift { Shift::Rrx } else { Shift::Ror(shift_operand) }),
            _ => panic!("Invalid shift type {shift_type}"),
        };
        operand2
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

pub fn undefined_arm(_opcode: u32) -> ArmInstructionInfo {
    todo!("Handle undefined instruction?")
}
