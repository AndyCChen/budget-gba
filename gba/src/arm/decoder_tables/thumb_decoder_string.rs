use crate::arm::decoder_tables::thumb_decoder::{AddSubOp, ThumbInstruction, AluOperation};

impl ThumbInstruction {
    pub fn to_asm_string(&self, _pc: u32) -> String {
        match self {
            ThumbInstruction::Lsl { shift, rs, rd } => format!("lsl r{rd}, r{rs}, #{shift}"),
            ThumbInstruction::Lsr { shift, rs, rd } => format!("lsr r{rd}, r{rs}, #{shift}"),
            ThumbInstruction::Asr { shift, rs, rd } => format!("asr r{rd}, r{rs}, #{shift}"),

            ThumbInstruction::Add { rd, rs, op } => match op {
                AddSubOp::Register(rn) => format!("add r{rd}, r{rs}, r{rn}"),
                AddSubOp::Immediate(expr) => format!("add r{rd}, r{rs}, #{expr}"),
            },
            ThumbInstruction::Sub { rd, rs, op } => match op {
                AddSubOp::Register(rn) => format!("sub r{rd}, r{rs}, r{rn}"),
                AddSubOp::Immediate(expr) => format!("sub r{rd}, r{rs}, #{expr}"),
            },

            ThumbInstruction::Mov { rd, offset } => format!("mov r{rd}, #{offset}"),
            ThumbInstruction::Cmp { rd, offset } => format!("cmp r{rd}, #{offset}"),
            ThumbInstruction::AddImm { rd, offset } => format!("add r{rd}, #{offset}"),
            ThumbInstruction::SubImm { rd, offset } => format!("sub r{rd}, #{offset}"),

            ThumbInstruction::AluOp { op, rs, rd } => {
                match op {
                    AluOperation::And => format!("and r{rd}, r{rs}"),
                    AluOperation::Eor => format!("eor r{rd}, r{rs}"),
                    AluOperation::Lsl => format!("lsl r{rd}, r{rs}"),
                    AluOperation::Lsr => format!("lsr r{rd}, r{rs}"),
                    AluOperation::Asr => format!("asr r{rd}, r{rs}"),
                    AluOperation::Adc => format!("adc r{rd}, r{rs}"),
                    AluOperation::Sbc => format!("sbc r{rd}, r{rs}"),
                    AluOperation::Ror => format!("ror r{rd}, r{rs}"),
                    AluOperation::Tst => format!("tst r{rd}, r{rs}"),
                    AluOperation::Neg => format!("neg r{rd}, r{rs}"),
                    AluOperation::Cmp => format!("cmp r{rd}, r{rs}"),
                    AluOperation::Cmn => format!("cmn r{rd}, r{rs}"),
                    AluOperation::Orr => format!("orr r{rd}, r{rs}"),
                    AluOperation::Mul => format!("mul r{rd}, r{rs}"),
                    AluOperation::Bic => format!("bic r{rd}, r{rs}"),
                    AluOperation::Mvn => format!("mvn r{rd}, r{rs}"),
                }
            }

            ThumbInstruction::AddHi { rd, rs } => format!("add r{rd}, r{rs}"),
            ThumbInstruction::CmpHi { rd, rs } => format!("cmp r{rd}, r{rs}"),
            ThumbInstruction::MovHi { rd, rs } => format!("mov r{rd}, r{rs}"),
            ThumbInstruction::BxHi { rs } => format!("bx r{rs}"),

            ThumbInstruction::PcRelativeLoad { rd, offset } => format!("ldr r{rd}, [PC, #{offset}]"),

            ThumbInstruction::Und { opcode } => format!("undefined 0x{opcode:08X}"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::arm::decoder_tables::decode_thumb;

    #[test]
    fn test_decode() {
        let opcode = (0b11 << 11) | (0 << 10) | (0 << 9) | (0x7 << 6) | (3 << 3) | 5;
        let thumb_string = decode_thumb(opcode).to_asm_string(16);
        println!("{thumb_string}")
    }
}
