use crate::arm::decoder_tables::thumb_decoder::{AddSubOp, ThumbInstruction};

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
