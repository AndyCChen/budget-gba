use crate::arm::opcode_tables::{ASR, LSL, LSR};

#[rustfmt::skip]
pub enum ThumbInstruction {
    Lsl { shift: u8, rs: u8, rd: u8 },
    Lsr { shift: u8, rs: u8, rd: u8 },
    Asr { shift: u8, rs: u8, rd: u8 },

    Add { rd: u8, rs: u8, op: AddSubOp },
    Sub { rd: u8, rs: u8, op: AddSubOp },

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

pub fn undefined_thumb(opcode: u16) -> ThumbInstruction {
    ThumbInstruction::Und { opcode }
}
