use bitflags::bitflags;

// 16.78 MHz clock rate
pub const ARM7TDMI_CLOCK_RATE: usize = 16 * 1024 * 1024;

#[repr(u8)]
pub enum ArmConditionCode {
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

bitflags! {
    #[derive(Clone, Copy)]
    pub struct AccessCode: u8 {
        const NONSEQUENTIAL = 0;
        const SEQUENTIAL = 1 << 0;
        const CODE = 1 << 1;
        const DMA = 1 << 2;
        const LOCK = 1 << 3;
    }
}

bitflags! {
    pub struct KindCode: u8 {
        const INSTRUCTION_READ = 0;
        const GENERAL_READ = 1 << 0;
        const WRITE = 1 << 1;
    }
}
