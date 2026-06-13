use crate::arm::opcode_tables::{ASR, LSL, LSR, to_negative, };

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

    LdrRegister { is_byte: bool, rd: u8, rb: u8, ro: u8 },
    StrRegister { is_byte: bool, rd: u8, rb: u8, ro: u8 },

    LoadSignedByteHalfword { is_byte: bool, is_signed: bool, rd: u8, rb: u8, ro: u8 },
    StoreHalfword { rd: u8, rb: u8, ro: u8 },

    LoadImm { is_byte: bool, rd: u8, rb: u8, offset: u8 },
    StoreImm { is_byte: bool, rd: u8, rb: u8, offset: u8 },

    LoadOffsetHalfword { rd: u8, rb: u8, offset: u8 },
    StoreOffsetHalfword { rd: u8, rb: u8, offset: u8 },

    LoadSpRelative { rd: u8, offset: u16 },
    StoreSpRelative { rd: u8, offset: u16 },

    PcSpLoad { is_stack_pointer: bool, rd: u8, offset: u16 },

    SpAddOffset { offset: i16 },

    Push{ transfer_sp_pc: bool, rlist: u8 },
    Pop { transfer_sp_pc: bool, rlist: u8 },

    Ldm { rb: u8, rlist: u8 },
    Stm { rb: u8, rlist: u8 },

    ConditionalBranch { cond: ConditionBranchType, offset: u32 },

    Swi { comment_field: u8 },

    UnconditionalBranch { offset: u32 },

    LongBranchLinkFirst { offset: u32 },
    LongBranchLinkSecond { offset: u32 },

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

pub fn load_store_register_offset(opcode: u16) -> ThumbInstruction {
    let is_load = (opcode >> 11) & 1 == 1;
    let is_byte = (opcode >> 10) & 1 == 1;

    let ro = ((opcode >> 6) & 0b111) as u8;
    let rb = ((opcode >> 3) & 0b111) as u8;
    let rd = (opcode & 0b111) as u8;

    match is_load {
        true => ThumbInstruction::LdrRegister { is_byte, rd, rb, ro },
        false => ThumbInstruction::StrRegister { is_byte, rd, rb, ro }
    }
}

pub fn load_store_sign_extended(opcode: u16) -> ThumbInstruction {
    let op = (opcode >> 10) & 0b11;
    let ro = ((opcode >> 6) & 0b111) as u8;
    let rb = ((opcode >> 3) & 0b111) as u8;
    let rd = (opcode & 0b111) as u8;

    match op {
        0 => ThumbInstruction::StoreHalfword { rd, rb, ro },
        1 => ThumbInstruction::LoadSignedByteHalfword { is_byte: false, is_signed: false, rd, rb, ro },
        2 => ThumbInstruction::LoadSignedByteHalfword { is_byte: true, is_signed: true, rd, rb, ro },
        3 => ThumbInstruction::LoadSignedByteHalfword { is_byte: false, is_signed: true, rd, rb, ro },
        _ => panic!("Invallid op: {op}!"),
    }
}

pub fn load_store_immediate_offset(opcode: u16) -> ThumbInstruction {
    let is_byte = (opcode >> 12) & 1 == 1;
    let is_load = (opcode >> 11) & 1 == 1;
    let offset = (((opcode >> 6) & 0x1F) as u8) << if is_byte { 0 } else { 2 };
    let rb = ((opcode >> 3) & 0b111) as u8;
    let rd = (opcode & 0b111) as u8;

    match is_load  {
        true => ThumbInstruction::LoadImm { is_byte, rd, rb, offset },
        false => ThumbInstruction::StoreImm { is_byte, rd, rb, offset },
    }
}

pub fn load_store_halfword_immediate_offset(opcode: u16) -> ThumbInstruction {
    let is_load = (opcode >> 11) & 1 == 1;
    let offset = (((opcode >> 6) & 0x1F) as u8) << 1;
    let rb = ((opcode >> 3) & 0b111) as u8;
    let rd = (opcode & 0b111) as u8;

    match is_load {
        true => ThumbInstruction::LoadOffsetHalfword { rd, rb, offset },
        false => ThumbInstruction::StoreOffsetHalfword { rd, rb, offset },
    }
}

pub fn sp_load_store_relative_offset(opcode: u16) -> ThumbInstruction {
    let is_load = (opcode >> 11) & 1 == 1;
    let rd = ((opcode >> 8) & 0b111) as u8;
    let offset = (opcode & 0xFF) << 2;

    match is_load {
        true => ThumbInstruction::LoadSpRelative { rd, offset },
        false => ThumbInstruction::StoreSpRelative { rd, offset },
    }
}

pub fn pc_sp_load_address(opcode: u16) -> ThumbInstruction {
    let is_stack_pointer = (opcode >> 1) & 1 == 1;
    let rd = ((opcode >> 8) & 0b111) as u8;
    let offset = (opcode & 0xFF) << 2;

    ThumbInstruction::PcSpLoad { is_stack_pointer, rd, offset }
}

pub fn add_sub_sp(opcode: u16) -> ThumbInstruction {
    let is_signed = (opcode >> 7) & 1 == 1;
    let mut offset = (opcode & 0x7F) << 2;

    if is_signed {
        offset = to_negative(offset);
    }

    ThumbInstruction::SpAddOffset { offset: offset as i16 }
}

pub fn push_pop_register(opcode: u16) -> ThumbInstruction {
    let is_pop = (opcode >> 11) & 1 == 1;
    let transfer_sp_pc = (opcode >> 8) & 1 == 1;
    let rlist = (opcode & 0xFF) as u8;

    match is_pop {
        true => ThumbInstruction::Pop { transfer_sp_pc, rlist },
        false => ThumbInstruction::Push { transfer_sp_pc, rlist },
    }
}

pub fn multiple_load_store(opcode: u16) -> ThumbInstruction {
    let is_load = (opcode >> 11) & 1 == 1;
    let rb = ((opcode >> 8) & 0b111) as u8;
    let rlist = (opcode & 0xFF) as u8;

    match is_load {
        true => ThumbInstruction::Ldm { rb, rlist },
        false => ThumbInstruction::Stm { rb, rlist },
    }
}

#[derive(Clone, Copy)]
pub enum ConditionBranchType {
    EQ,
    NE,
    CS,
    CC,
    MI,
    PL,
    VS,
    VC,
    HI,
    LS,
    GE,
    LT,
    GT,
    LE,
}

impl TryFrom<u8> for ConditionBranchType  {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ConditionBranchType::EQ),
            1 => Ok(ConditionBranchType::NE),
            2 => Ok(ConditionBranchType::CS),
            3 => Ok(ConditionBranchType::CC),
            4 => Ok(ConditionBranchType::MI),
            5 => Ok(ConditionBranchType::PL),
            6 => Ok(ConditionBranchType::VS),
            7 => Ok(ConditionBranchType::VC),
            8 => Ok(ConditionBranchType::HI),
            9 => Ok(ConditionBranchType::LS),
            10 => Ok(ConditionBranchType::GE),
            11 => Ok(ConditionBranchType::LT),
            12 => Ok(ConditionBranchType::GT),
            13 => Ok(ConditionBranchType::LE), 
            14 => Err("Condition 14 is undefined!"),
            15 => Err("Condition 15 defines SWI instruction"),
            _ => Err("Invalid condition"),
        }
    }
}

pub fn conditional_branch(opcode: u16) -> ThumbInstruction {
    let cond = ((opcode >> 8) & 0xF) as u8;
    let mut offset = u32::from((opcode & 0xFF) << 1);

    // negative
    if offset & 0x100 != 0 {
        offset |= 0xFFFF_FF00;
    }

    let cond = ConditionBranchType::try_from(cond).unwrap();
    ThumbInstruction::ConditionalBranch { cond, offset }
}

pub fn software_interrupt(opcode: u16) -> ThumbInstruction {
    ThumbInstruction::Swi { comment_field: (opcode & 0xFF) as u8 }
}

pub fn unconditional_branch(opcode: u16) -> ThumbInstruction {
    let mut offset = u32::from((opcode & 0x7FF) << 1);

    // negative
    if offset & 0x800 != 0 {
        offset |= 0xFFFF_F800;
    } 

    ThumbInstruction::UnconditionalBranch { offset }
}

// todo: figure out way to decode this 32 bit instruction which is split into 2 16 bit instructions
// need to store some intermediate state to hold previous address to form the full branch target address.
pub fn long_branch_with_link(opcode: u16) -> ThumbInstruction {
    let offset_lo = (opcode >> 11) & 1 == 1;
    let offset = u32::from(opcode & 0x7FF);

    match offset_lo {
        true => ThumbInstruction::LongBranchLinkSecond { offset: offset << 1 },
        false => ThumbInstruction::LongBranchLinkFirst { offset: offset << 12 },
    }
}

pub fn undefined_thumb(opcode: u16) -> ThumbInstruction {
    ThumbInstruction::Und { opcode }
}
