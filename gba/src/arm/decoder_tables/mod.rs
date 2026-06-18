mod arm_decoder;
mod arm_decoder_gen;
mod arm_decoder_string;
mod decoder_ringbuffer;
mod thumb_decoder;
mod thumb_decoder_gen;
mod thumb_decoder_string;

use crate::arm::opcode_tables::{ARM_TABLE_SIZE, THUMB_TABLE_SIZE};

use arm_decoder::ArmInstructionInfo;
use arm_decoder_gen::{ArmDecoder, generate_arm_decoder_table};
pub use decoder_ringbuffer::RingBuffer;
use thumb_decoder::ThumbInstruction;
use thumb_decoder_gen::{ThumbDecoder, generate_thumb_decoder_table};

static ARM_DECODER: [ArmDecoder; ARM_TABLE_SIZE] = generate_arm_decoder_table();
static THUMB_DECODER: [ThumbDecoder; THUMB_TABLE_SIZE] = generate_thumb_decoder_table();

pub fn decode_arm(opcode: u32) -> ArmInstructionInfo {
    let arm_table_hash = ((opcode & 0x0FF00000) >> 16) | ((opcode & 0xF0) >> 4);
    ARM_DECODER[arm_table_hash as usize](opcode)
}

pub fn decode_thumb(opcode: u16) -> ThumbInstruction {
    let thumb_table_hash = (opcode >> 6) & 0x3FF;
    THUMB_DECODER[thumb_table_hash as usize](opcode)
}

#[repr(u8)]
enum ArmConditionCode {
    /// Z set
    EQ = 0b0000,
    /// Z clear
    NE = 0b0001,
    /// C set
    CS = 0b0010,
    /// C clear
    CC = 0b0011,
    /// N set
    MI = 0b0100,
    /// N clear
    PL = 0b0101,
    /// V set
    VS = 0b0110,
    /// V clear
    VC = 0b0111,
    /// C set & Z clear
    HI = 0b1000,
    /// C clear or Z set
    LS = 0b1001,
    /// N equals V
    GE = 0b1010,
    /// N not equal V
    LT = 0b1011,
    /// Z clear AND (N equals V)
    GT = 0b1100,
    /// Z set OR (N not equal V)
    LE = 0b1101,
    /// always
    AL = 0b1110,
}

impl TryFrom<u8> for ArmConditionCode {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0b0000 => Ok(ArmConditionCode::EQ),
            0b0001 => Ok(ArmConditionCode::NE),
            0b0010 => Ok(ArmConditionCode::CS),
            0b0011 => Ok(ArmConditionCode::CC),
            0b0100 => Ok(ArmConditionCode::MI),
            0b0101 => Ok(ArmConditionCode::PL),
            0b0110 => Ok(ArmConditionCode::VS),
            0b0111 => Ok(ArmConditionCode::VC),
            0b1000 => Ok(ArmConditionCode::HI),
            0b1001 => Ok(ArmConditionCode::LS),
            0b1010 => Ok(ArmConditionCode::GE),
            0b1011 => Ok(ArmConditionCode::LT),
            0b1100 => Ok(ArmConditionCode::GT),
            0b1101 => Ok(ArmConditionCode::LE),
            0b1110 => Ok(ArmConditionCode::AL),
            _ => Err(value),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_thumb() {
        let lo = decode_thumb(0xF000).to_asm_string(0x890 + 4);
        let hi = decode_thumb(0xF95C).to_asm_string(0x892 + 4);
        println!("{lo}");
        println!("{hi}");
    }
}
