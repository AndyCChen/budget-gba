use crate::arm::constants::arm_condition_code::*;
use crate::arm::decoder_tables::arm_decoder::{ArmInstruction, ArmInstructionInfo};

impl ArmInstructionInfo {
    fn to_string(&self, pc: u32) -> String {
        let condition_code_str = get_condition_str(self.condition);

        match self.instruction {
            ArmInstruction::BX { rn } => format!("BX{condition_code_str} R{rn}"),
            ArmInstruction::B { offset } => {
                let dest = pc.wrapping_add(offset);
                format!("B{condition_code_str} {dest}")
            }
            ArmInstruction::BL { offset } => format!("BL{condition_code_str} {offset}"),
            ArmInstruction::MOV { set_condition, rd } => todo!(),
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
