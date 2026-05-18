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
            Bx { rn } =>     format!("bx{cond_str} R{rn}"),
            B { offset } =>  format!("b{cond_str} 0x{:08X}", pc.wrapping_add(offset)),
            Bl { offset } => format!("bl{cond_str} 0x{:08X}", pc.wrapping_add(offset)),

            // data processing
            And { set_condition, rd, rn, op2 } => format_data_processing_mode_2("and", cond_str, set_condition, rd, rn, op2),
            Eor { set_condition, rd, rn, op2 } => format_data_processing_mode_2("eor", cond_str, set_condition, rd, rn, op2),
            Sub { set_condition, rd, rn, op2 } => format_data_processing_mode_2("sub", cond_str, set_condition, rd, rn, op2),
            Rsb { set_condition, rd, rn, op2 } => format_data_processing_mode_2("rsb", cond_str, set_condition, rd, rn, op2),
            Add { set_condition, rd, rn, op2 } => format_data_processing_mode_2("add", cond_str, set_condition, rd, rn, op2),
            Adc { set_condition, rd, rn, op2 } => format_data_processing_mode_2("adc", cond_str, set_condition, rd, rn, op2),
            Sbc { set_condition, rd, rn, op2 } => format_data_processing_mode_2("sbc", cond_str, set_condition, rd, rn, op2),
            Rsc { set_condition, rd, rn, op2 } => format_data_processing_mode_2("rsc", cond_str, set_condition, rd, rn, op2),
            Tst { rn, op2 } =>                    format_data_processing_mode_1("tst", cond_str, rn, op2),
            Teq { rn, op2 } =>                    format_data_processing_mode_1("teq", cond_str, rn, op2),
            Cmp { rn, op2 } =>                    format_data_processing_mode_1("cmp", cond_str, rn, op2),
            Cmn { rn, op2 } =>                    format_data_processing_mode_1("cmn", cond_str, rn, op2),
            Orr { set_condition, rd, rn, op2 } => format_data_processing_mode_2("orr", cond_str, set_condition, rd, rn, op2),
            Mov { set_condition, rd, op2 } =>     format_data_processing_mode_0("mov", cond_str, set_condition, rd, op2),
            Bic { set_condition, rd, rn, op2 } => format_data_processing_mode_2("bic", cond_str, set_condition, rd, rn, op2),
            Mvn { set_condition, rd, op2 } =>     format_data_processing_mode_0("mvn", cond_str, set_condition, rd, op2),

            // mrs and msr instructions
            Mrs { spsr_dest, rd } =>                  format!("mrs{cond_str} r{rd}, {}", if spsr_dest { "SPSR" } else { "CPSR" }),
            Msr { spsr_dest, rm } =>                  format!("msr{cond_str} {}, r{rm}", if spsr_dest { "SPSR" } else { "CPSR" }),
            MsrFlagOnly { source_operand, spsr_dest } => {
                match source_operand {
                    ShiftOperand::Register(rm) =>     format!("msr{cond_str} {}, r{rm}",   if spsr_dest { "SPSR_flg" } else { "CPSR_flg" }),
                    ShiftOperand::Expression(expr) => format!("msr{cond_str} {}, #{expr}", if spsr_dest { "SPSR_flg" } else { "CPSR_flg" }),
                }
            }

            // multiply
            Mul { set_condition, rd, rm, rs } =>     format!("mul{cond_str}{} r{rd}, r{rm}, {rs}",         if set_condition { "s" } else { "" }),
            Mla { set_condition, rd, rm, rs, rn } => format!("mla{cond_str}{} r{rd}, r{rm}, r{rs}, r{rn}", if set_condition { "s" } else { "" }),
       
            // multiply long
            Mull { set_condition, is_signed, rdlo, rdhi, rm, rs } => format!("{}mull{cond_str}{} r{rdlo}, r{rdhi}, r{rm}, r{rs}", if set_condition { "s" } else { "" }, if is_signed { "s" } else { "" }),
            Mlal { set_condition, is_signed, rdlo, rdhi, rm, rs } => format!("{}mlal{cond_str}{} r{rdlo}, r{rdhi}, r{rm}, r{rs}", if set_condition { "s" } else { "" }, if is_signed { "s" } else { "" }),
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
    let s = if set_condition { "s" } else { "" };

    #[rustfmt::skip]
    let arm_str = match op2 {
        ArmDataOp2::Expression(expr) =>                       format!("{mnemonic}{cond_str}{s} r{rd},#{expr}"),
        ArmDataOp2::Rm(rm, shift) => {
            match shift {
                Shift::None =>                                format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}"),

                Shift::Lsl(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, lsl #{expr}"),
                Shift::Lsl(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, lsl r{rs}"),

                Shift::Lsr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, lsr #{expr}"),
                Shift::Lsr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, lsr r{rs}"),

                Shift::Asr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, asr #{expr}"),
                Shift::Asr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, asr r{rs}"),

                Shift::Ror(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, ror #{expr}"),
                Shift::Ror(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, ror r{rs}"),

                Shift::Rrx =>                                 format!("{mnemonic}{cond_str}{s} r{rd}, r{rm}, rrX"),
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
        ArmDataOp2::Expression(expr) =>                       format!("{mnemonic}{cond_str}s r{rn}, #{expr}"),
        ArmDataOp2::Rm(rm, shift) => {
            match shift {
                Shift::None =>                                format!("{mnemonic}{cond_str}s r{rn}, r{rm}"),

                Shift::Lsl(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}s r{rn}, r{rm}, lsl #{expr}"),
                Shift::Lsl(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}s r{rn}, r{rm}, lsl r{rs}"),

                Shift::Lsr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}s r{rn}, r{rm}, lsr #{expr}"),
                Shift::Lsr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}s r{rn}, r{rm}, lsr r{rs}"),

                Shift::Asr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}s r{rn}, r{rm}, asr #{expr}"),
                Shift::Asr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}s r{rn}, r{rm}, asr r{rs}"),

                Shift::Ror(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}s r{rn}, r{rm}, ror #{expr}"),
                Shift::Ror(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}s r{rn}, r{rm}, ror r{rs}"),

                Shift::Rrx =>                                 format!("{mnemonic}{cond_str}s r{rn}, r{rm}, rrx"),
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
    let s = if set_condition { "s" } else { "" };

    #[rustfmt::skip]
    let arm_str = match op2 {
        ArmDataOp2::Expression(expr) =>                       format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, #{expr}"),
        ArmDataOp2::Rm(rm, shift) => {
            match shift {
                Shift::None =>                                format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, r{rm}"),

                Shift::Lsl(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, lsl #{expr}"),
                Shift::Lsl(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, r{rm}, lsl r{rs}"),

                Shift::Lsr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, lsr #{expr}"),
                Shift::Lsr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, r{rm}, lsr r{rs}"),

                Shift::Asr(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, asr #{expr}"),
                Shift::Asr(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, r{rm}, asr r{rs}"),

                Shift::Ror(ShiftOperand::Expression(expr)) => format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, ror #{expr}"),
                Shift::Ror(ShiftOperand::Register(rs)) =>     format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, r{rm}, ror r{rs}"),

                Shift::Rrx =>                                 format!("{mnemonic}{cond_str}{s} r{rd}, r{rn}, rrx"),
            }
        },
    };
    arm_str
}

fn get_condition_str(condition_code: u8) -> &'static str {
    match condition_code {
        EQ => "eq",
        NE => "ne",
        CS => "cs",
        CC => "cc",
        MI => "mi",
        PL => "pl",
        VS => "vs",
        VC => "vc",
        HI => "hi",
        LS => "ls",
        GE => "ge",
        LT => "lt",
        GT => "gt",
        LE => "le",
        AL => "",
        _ => panic!("Invalid condition code! {condition_code}"),
    }
}
