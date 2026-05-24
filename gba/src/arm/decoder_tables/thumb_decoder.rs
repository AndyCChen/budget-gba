use crate::arm::opcode_tables::{ASR, LSL, LSR};

#[rustfmt::skip]
pub enum ThumbInstruction {
    Lsl { shift: u8, rs: u8, rd: u8 },
    Lsr { shift: u8, rs: u8, rd: u8 },
    Asr { shift: u8, rs: u8, rd: u8 },

    Add { rd: u8, rs: u8, op: AddSubOp },
    Sub { rd: u8, rs: u8, op: AddSubOp },

    Mov {rd: u8, offset: u8 },
    Cmp {rd: u8, offset: u8 },
    AddImm {rd: u8, offset: u8 },
    SubImm {rd: u8, offset: u8 },

    AluOp { op: AluOperation, rs: u8, rd: u8 },

    AddHi { rd: u8, rs: u8 },
    CmpHi { rd: u8, rs: u8 },
    MovHi { rd: u8, rs: u8 },
    BxHi { rs: u8 },

    PcRelativeLoad { rd: u8, offset: u16 },

    Und { opcode: u16 },
}

pub fn move_shifted(opcode: u16) -> ThumbInstruction {
    let op = (opcode >> 11) & 3;
    let mut shift = ((opcode >> 6) & 0x1F) as u8;
    let rs = ((opcode >> 3) & 7) as u8;
    let rd = (opcode & 7) as u8;

    let zero_shift = shift == 0;

    if zero_shift && matches!(op as u8, LSR | ASR) {
        shift = 32;
    }

    match op as u8 {
        LSL => ThumbInstruction::Lsl { shift, rs, rd },
        LSR => ThumbInstruction::Lsr { shift, rs, rd },
        ASR => ThumbInstruction::Asr { shift, rs, rd },
        _ => panic!("Invalid op: {op}"),
    }
}

#[derive(Clone, Copy)]
pub enum AddSubOp {
    Register (u8),
    Immediate(u8),
}

pub fn add_subtract(opcode: u16) -> ThumbInstruction {
    let is_immediate = (opcode >> 10) & 1 == 1;
    let is_sub = (opcode >> 9) & 1 == 1;
    let rn_offset = ((opcode >> 6) & 7) as u8;
    let rs = ((opcode >> 3) & 7) as u8;
    let rd = (opcode & 7) as u8;

    let op = match is_immediate {
        true => AddSubOp::Immediate(rn_offset),
        false => AddSubOp::Register(rn_offset),
    };

    #[rustfmt::skip]    
    let instruction = match is_sub {
        true =>  ThumbInstruction::Sub { rd, rs, op },
        false => ThumbInstruction::Add { rd, rs, op },
    };

    instruction
}

pub fn mov_cmp_add_sub_immediate(opcode: u16) -> ThumbInstruction {
    const MOV: u8 = 0;
    const CMP: u8 = 1;
    const ADD: u8 = 2;
    const SUB: u8 = 3;

    let op = (opcode >> 11) & 0b11;
    let rd = ((opcode >> 8) & 0b111) as u8;
    let offset =(opcode & 0xFF) as u8;

    match op as u8{
        MOV => ThumbInstruction::Mov { rd, offset },
        CMP => ThumbInstruction::Cmp { rd, offset },
        ADD => ThumbInstruction::AddImm { rd, offset },
        SUB => ThumbInstruction::SubImm { rd, offset },
        _ => panic!("Invalid OP: {op}!")
    }
}

pub enum AluOperation {
    And,
    Eor,
    Lsl,
    Lsr,
    Asr,
    Adc,
    Sbc,
    Ror,
    Tst,
    Neg,
    Cmp,
    Cmn,
    Orr,
    Mul,
    Bic,
    Mvn,
}

pub fn alu_operations(opcode: u16) -> ThumbInstruction {
    let op = ((opcode >> 6) & 0xF) as u8;
    let rs = ((opcode >> 3) & 0b111) as u8;
    let rd = (opcode & 0b111) as u8;

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

    let op = match op {
        AND => AluOperation::And,
        EOR => AluOperation::Eor,
        LSL => AluOperation::Lsl,
        LSR => AluOperation::Lsr,
        ASR => AluOperation::Asr,
        ADC => AluOperation::Adc,
        SBC => AluOperation::Sbc,
        ROR => AluOperation::Ror,
        TST => AluOperation::Tst,
        NEG => AluOperation::Neg,
        CMP => AluOperation::Cmp,
        CMN => AluOperation::Cmn,
        ORR => AluOperation::Orr,
        MUL => AluOperation::Mul,
        BIC => AluOperation::Bic,
        MVN => AluOperation::Mvn,
        _ => panic!("Invalid alu op: {op}!")
    };

    ThumbInstruction::AluOp { op, rs, rd }
}

pub fn add_cmp_mov_hi(opcode: u16) -> ThumbInstruction {
    const ADD: u8 = 0;
    const CMP: u8 = 1;
    const MOV: u8 = 2;
    const BX: u8 = 3;

    let op = (opcode >> 8) & 0b11;
    let h1 = (opcode >> 7) & 1 == 1;
    let h2 = (opcode >> 6) & 1 == 1;

    let rd = ((u32::from(h1) << 3) | u32::from(opcode & 0x7)) as u8;
    let rs = ((u32::from(h2) << 3) | u32::from((opcode >> 3) & 0x7)) as u8;

    match op as u8 {
        ADD => ThumbInstruction::AddHi { rd, rs },
        CMP => ThumbInstruction::CmpHi { rd, rs },
        MOV => ThumbInstruction::MovHi { rd, rs },
        BX => ThumbInstruction::BxHi { rs },
        _ => panic!("Invalid op: {op}!"),
    }
}

pub fn pc_relative_load(opcode: u16) -> ThumbInstruction {
    let rd = ((opcode >> 8) & 0b111) as u8;
    let offset = (opcode & 0xFF) << 2;

    ThumbInstruction::PcRelativeLoad { rd, offset }
}

pub fn undefined_thumb(opcode: u16) -> ThumbInstruction {
    ThumbInstruction::Und { opcode }
}
