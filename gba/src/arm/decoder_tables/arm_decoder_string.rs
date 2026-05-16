use crate::arm::constants::arm_condition_code::*;
use crate::arm::decoder_tables::arm_decoder::{
    ArmDataOp2, ArmInstruction::*, ArmInstructionInfo, Shift, ShiftOperand,
};

impl ArmInstructionInfo {
    pub fn to_asm_string(&self, pc: u32) -> String {
        let cond_str = get_condition_str(self.condition);

        #[rustfmt::skip]
        let arm_str = match self.instruction {
            // branches
            Bx { rn } =>     format!("BX{cond_str} R{rn}"),
            B { offset } =>  format!("B{cond_str} 0x{:08X}", pc.wrapping_add(offset)),
            Bl { offset } => format!("BL{cond_str} 0x{:08X}", pc.wrapping_add(offset)),

            // data processing
            And { set_condition, rd, rn, op2 } => format_data_processing_mode_2("AND", cond_str, set_condition, rd, rn, op2),
            Eor { set_condition, rd, rn, op2 } => format_data_processing_mode_2("EOR", cond_str, set_condition, rd, rn, op2),
            Sub { set_condition, rd, rn, op2 } => format_data_processing_mode_2("SUB", cond_str, set_condition, rd, rn, op2),
            Rsb { set_condition, rd, rn, op2 } => format_data_processing_mode_2("RSB", cond_str, set_condition, rd, rn, op2),
            Add { set_condition, rd, rn, op2 } => format_data_processing_mode_2("ADD", cond_str, set_condition, rd, rn, op2),
            Adc { set_condition, rd, rn, op2 } => format_data_processing_mode_2("ADC", cond_str, set_condition, rd, rn, op2),
            Sbc { set_condition, rd, rn, op2 } => format_data_processing_mode_2("SBC", cond_str, set_condition, rd, rn, op2),
            Rsc { set_condition, rd, rn, op2 } => format_data_processing_mode_2("RSC", cond_str, set_condition, rd, rn, op2),
            Tst { rn, op2 } =>                    format_data_processing_mode_1("TST", cond_str, rn, op2),
            Teq { rn, op2 } =>                    format_data_processing_mode_1("TEQ", cond_str, rn, op2),
            Cmp { rn, op2 } =>                    format_data_processing_mode_1("CMP", cond_str, rn, op2),
            Cmn { rn, op2 } =>                    format_data_processing_mode_1("CMN", cond_str, rn, op2),
            Orr { set_condition, rd, rn, op2 } => format_data_processing_mode_2("ORR", cond_str, set_condition, rd, rn, op2),
            Mov { set_condition, rd, op2 } =>     format_data_processing_mode_0("MOV", cond_str, set_condition, rd, op2),
            Bic { set_condition, rd, rn, op2 } => format_data_processing_mode_2("BIC", cond_str, set_condition, rd, rn, op2),
            Mvn { set_condition, rd, op2 } =>     format_data_processing_mode_0("MVN", cond_str, set_condition, rd, op2),
        };
        arm_str
    }
}

#[inline]
fn format_data_processing_mode_0(
    mnemonic: &str,
    cond_str: &str,
    set_condition: bool,
    rd: u8,
    op2: ArmDataOp2,
) -> String {
    let s = if set_condition { "S" } else { "" };

    #[rustfmt::skip]
    let arm_str = match op2 {
        ArmDataOp2::Expression(expr) =>                       format!("{mnemonic}{cond_str} {s} R{rd},#{expr}"),
        ArmDataOp2::Rm(rm, shift) => {
            match shift {
                Shift::None =>                                format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}"),

                Shift::Lsl(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, Lsl #{expr}"),
                Shift::Lsl(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, Lsl R{rs}"),

                Shift::Lsr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, Lsr #{expr}"),
                Shift::Lsr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, Lsr R{rs}"),

                Shift::Asr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, Asr #{expr}"),
                Shift::Asr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, Asr R{rs}"),

                Shift::Ror(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, Ror #{expr}"),
                Shift::Ror(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, Ror R{rs}"),

                Shift::Rxx =>                                 format!("{mnemonic}{cond_str} {s} R{rd}, R{rm}, RRX"),
            }
        },
    };
    arm_str
}

#[inline]
fn format_data_processing_mode_1(
    mnemonic: &str,
    cond_str: &str,
    rn: u8,
    op2: ArmDataOp2,
) -> String {
    #[rustfmt::skip]
    let arm_str = match op2 {
        ArmDataOp2::Expression(expr) =>                       format!("{mnemonic}{cond_str}S R{rn}, #{expr}"),
        ArmDataOp2::Rm(rm, shift) => {
            match shift {
                Shift::None =>                                format!("{mnemonic}{cond_str}S R{rn}, R{rm}"),

                Shift::Lsl(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}S R{rn}, R{rm}, Lsl #{expr}"),
                Shift::Lsl(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}S R{rn}, R{rm}, Lsl R{rs}"),

                Shift::Lsr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}S R{rn}, R{rm}, Lsr #{expr}"),
                Shift::Lsr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}S R{rn}, R{rm}, Lsr R{rs}"),

                Shift::Asr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}S R{rn}, R{rm}, Asr #{expr}"),
                Shift::Asr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}S R{rn}, R{rm}, Asr R{rs}"),

                Shift::Ror(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}S R{rn}, R{rm}, Ror #{expr}"),
                Shift::Ror(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}S R{rn}, R{rm}, Ror R{rs}"),

                Shift::Rxx =>                                 format!("{mnemonic}{cond_str}S R{rn}, R{rm}, RRX"),
            }
        },
    };
    arm_str
}

#[inline]
fn format_data_processing_mode_2(
    mnemonic: &str,
    cond_str: &str,
    set_condition: bool,
    rd: u8,
    rn: u8,
    op2: ArmDataOp2,
) -> String {
    let s = if set_condition { "S" } else { "" };

    #[rustfmt::skip]
    let arm_str = match op2 {
        ArmDataOp2::Expression(expr) =>                       format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, #{expr}"),
        ArmDataOp2::Rm(rm, shift) => {
            match shift {
                Shift::None =>                                format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, R{rm}"),

                Shift::Lsl(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, Lsl #{expr}"),
                Shift::Lsl(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, R{rm}, Lsl R{rs}"),

                Shift::Lsr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, Lsr #{expr}"),
                Shift::Lsr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, R{rm}, Lsr R{rs}"),

                Shift::Asr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, Asr #{expr}"),
                Shift::Asr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, R{rm}, Asr R{rs}"),

                Shift::Ror(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, Ror #{expr}"),
                Shift::Ror(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, R{rm}, Ror R{rs}"),

                Shift::Rxx =>                                 format!("{mnemonic}{cond_str} {s} R{rd}, R{rn}, RRX"),
            }
        },
    };
    arm_str
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
