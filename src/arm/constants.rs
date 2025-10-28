pub mod arm_condition_code {
    /// Z set
    pub const EQ: u8 = 0b0000;
    /// Z clear
    pub const NE: u8 = 0b0001;
    /// C set
    pub const CS: u8 = 0b0010;
    /// C clear
    pub const CC: u8 = 0b0011;
    /// N set
    pub const MI: u8 = 0b0100;
    /// N clear
    pub const PL: u8 = 0b0101;
    /// V set
    pub const VS: u8 = 0b0110;
    /// V clear
    pub const VC: u8 = 0b0111;
    /// C set & Z clear
    pub const HI: u8 = 0b1000;
    /// C clear or Z set
    pub const LS: u8 = 0b1001;
    /// N equals V
    pub const GE: u8 = 0b1010;
    /// N not equal V
    pub const LT: u8 = 0b1011;
    /// Z clear AND (N equals V)
    pub const GT: u8 = 0b1100;
    /// Z set OR (N not equal V)
    pub const LE: u8 = 0b1101;
    /// always
    pub const AL: u8 = 0b1110;
}

pub mod access_code {
    pub const NONSEQUENTIAL: u8 = 0;
    pub const SEQUENTIAL: u8 = 1 << 0;
    pub const CODE: u8 = 1 << 1;
    pub const DMA: u8 = 1 << 2;
    pub const LOCK: u8 = 1 << 3;

    // pub const INSTRUCTION_READ: u8 = 1 << 4;
    // pub const GENERAL_READ: u8 = 1 << 5;
    // pub const WRITE: u8 = 1 << 6;
}

pub mod kind_code {
    pub const INSTRUCTION_READ: u8 = 0;
    pub const GENERAL_READ: u8 = 1 << 0;
    pub const WRITE: u8 = 1 << 1;
}
