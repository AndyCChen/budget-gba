use crate::arm::constants::arm_condition_code::*;
use crate::arm::decoder_tables::arm_decoder::{ArmInstruction::*, ArmInstructionInfo};

impl ArmInstructionInfo {
    pub fn to_asm_string(&self, pc: u32) -> String {
        let cond_str = get_condition_str(self.condition);

        match self.instruction {
            BX { rn } => format!("BX{cond_str} R{rn}"),
            B { offset } => {
                format!("B{cond_str} 0x{:08X}", pc.wrapping_add(offset))
            }
            BL { offset } => {
                format!("BL{cond_str} 0x{:08X}", pc.wrapping_add(offset))
            }

            MOV { set_condition, rd } => {
                format!("MOV{cond_str}")
            },
        }
    }
}

fn get_condition_str(condition_code: u8) -> &'static str {
    match condition_code {
        EQ => "EQ",
        NE => "NE",
        CS => "CS",
        CC => "CC",
        MI => "MI",
        PL => "PL",
        VS => "VS",
        VC => "VC",
        HI => "HI",
        LS => "LS",
        GE => "GE",
        LT => "LT",
        GT => "GT",
        LE => "LE",
        AL => "",
        _ => panic!("Invalid condition code! {condition_code}"),
    }
}
