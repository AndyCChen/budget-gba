use crate::arm::constants::arm_condition_code::*;
use crate::arm::decoder_tables::arm_decoder::{
    ArmDataOp2, ArmInstruction::*, ArmInstructionInfo, Shift, ShiftOperand,
};
use super::arm_decoder::{LdrStrAddress, LdrStrAddressShift, LdrhStrhAddress, BlockTransferAddressing}; 

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
        
            // load / store
            Ldr { is_byte, rd, address } => format_single_data_transfer("ldr", cond_str, pc, is_byte, rd, address),
            Str { is_byte, rd, address } => format_single_data_transfer("str", cond_str, pc, is_byte, rd, address),
  
            // load byte/halfword & store halfword
            Ldrh { rd, address } =>  format_halfword_and_signed_data_transfer("ldr", cond_str, pc, "h", rd, address),
            Strh { rd, address } =>  format_halfword_and_signed_data_transfer("str", cond_str, pc, "h", rd, address),
            Ldrsb { rd, address } => format_halfword_and_signed_data_transfer("ldr", cond_str, pc, "sb", rd, address),
            Ldrsh { rd, address } => format_halfword_and_signed_data_transfer("ldr", cond_str, pc, "sh", rd, address),

            // block transfers
            Ldm { addressing_mode, rn, is_write_back, rlist, is_s_bit } => format_block_transfer("ldm", cond_str, addressing_mode, rn, is_write_back, rlist, is_s_bit),
            Stm { addressing_mode, rn, is_write_back, rlist, is_s_bit } => format_block_transfer("stm", cond_str, addressing_mode, rn, is_write_back, rlist, is_s_bit),
        };
        arm_str
    }
}

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
        ArmDataOp2::Rm{ rm, shift } => {
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

fn format_data_processing_mode_1(
    mnemonic: &str,
    cond_str: &str,
    rn: u8,
    op2: ArmDataOp2,
) -> String {
    #[rustfmt::skip]
    let arm_str = match op2 {
        ArmDataOp2::Expression(expr) =>                       format!("{mnemonic}{cond_str}s r{rn}, #{expr}"),
        ArmDataOp2::Rm{ rm, shift } => {
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
        ArmDataOp2::Rm{ rm, shift } => {
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

fn format_single_data_transfer(
    mnemonic: &str, 
    cond_str: &str, 
    pc: u32, 
    is_byte: bool, 
    rd: u8, 
    address: LdrStrAddress
) -> String {
    use LdrStrAddress::*;
    use LdrStrAddressShift::*;

    let b = if is_byte {"b"} else {""};

    #[rustfmt::skip]
    let arm_str = match address {
        PcRelative(expr) =>                           format!("{mnemonic}{cond_str}{b} r{rd}, 0x{:08X}", pc.wrapping_add(expr)),
        PreIndexZero { rn } | PostIndexZero { rn } => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}]"),
        
        PreIndexExpression { rn, is_increment, expr, is_write_back } => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}, {}#{expr}]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),
        PostIndexExpression { rn, is_increment, expr } =>               format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}], {}#{expr}", if is_increment {"+"} else {"-"}),

        PreIndexShifted { rn, is_increment, rm, shift, is_write_back } => {
            match shift {
                None =>      format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}, {}r{rm}]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),

                Lsl(expr) => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}, {}r{rm}, lsl #{expr}]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),
                Lsr(expr) => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}, {}r{rm}, lsr #{expr}]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),
                Asr(expr) => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}, {}r{rm}, asr #{expr}]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),
                Ror(expr) => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}, {}r{rm}, ror #{expr}]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),

                Rrx =>       format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}, {}r{rm}, rrx]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),
            }
        },
        
        PostIndexShifted { rn, is_increment, rm, shift } => {
            match shift {
                None =>      format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}, {}r{rm}]", if is_increment {"+"} else {"-"}),

                Lsl(expr) => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}], {}r{rm}, lsl #{expr}", if is_increment {"+"} else {"-"}),
                Lsr(expr) => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}], {}r{rm}, lsr #{expr}", if is_increment {"+"} else {"-"}),
                Asr(expr) => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}], {}r{rm}, asr #{expr}", if is_increment {"+"} else {"-"}),
                Ror(expr) => format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}], {}r{rm}, ror #{expr}", if is_increment {"+"} else {"-"}),

                Rrx =>       format!("{mnemonic}{cond_str}{b} r{rd}, [r{rn}], {}r{rm}, rrx", if is_increment {"+"} else {"-"}),
            }
        },
    };

   arm_str
}

fn format_halfword_and_signed_data_transfer(
    mnemonic: &str, 
    cond_str: &str, 
    pc: u32, 
    mode: &str, 
    rd: u8, 
    address: LdrhStrhAddress
) -> String  {
    use LdrhStrhAddress::*;

     match address {
        PcRelative(expr) =>                           format!("{mnemonic}{cond_str}{mode} r{rd}, #{}", pc.wrapping_add(expr)),
        PreIndexZero { rn } | PostIndexZero { rn } => format!("{mnemonic}{cond_str}{mode} r{rd}, [r{rn}]"),

        PreIndexExpression { rn, is_increment, expr, is_write_back } => format!("{mnemonic}{cond_str}{mode} r{rd}, [r{rn}, {}#{expr}]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),
        PostIndexExpression { rn, is_increment, expr } =>               format!("{mnemonic}{cond_str}{mode} r{rd}, [r{rn}], {}#{expr}", if is_increment {"+"} else {"-"}),

        PreIndexRegister { rn, is_increment, rm, is_write_back } =>     format!("{mnemonic}{cond_str}{mode} r{rd}, [r{rn}, {}r{rm}]{}", if is_increment {"+"} else {"-"}, if is_write_back {"!"} else {""}),
        PostIndexRegister { rn, is_increment, rm } =>                   format!("{mnemonic}{cond_str}{mode} r{rd}, [r{rn}], {}r{rm}", if is_increment {"+"} else {"-"}),
    }
}

fn format_block_transfer(
    mnemonic: &str, 
    cond_str: &str, 
    addressing_mode: BlockTransferAddressing,
    rn: u8, 
    is_write_back: bool, 
    rlist: u16, 
    is_s_bit: bool 
) -> String {
    use BlockTransferAddressing::*;

    let write_back = if is_write_back {"!"} else {""};
    let s_bit = if is_s_bit {"^"} else {""};

    enum RegisterList {
        Range{ lo: u8, hi: u8 },
        Single(u8)
    }

    let mut register_list = Vec::with_capacity(16);
    let mut stack = Vec::with_capacity(16);

    for b in 0..16 {
        if (rlist >> b) & 1 == 1 {
            stack.push(b);
        } else if let Some(last) = stack.last() {
            if stack.len() <= 2 {
                for reg_num in stack.iter().copied() {
                    register_list.push(RegisterList::Single(reg_num))
                }
                stack.clear();
            } else {
                let hi = *last;
                stack.truncate(1);
                let lo = stack.pop().expect("Expect 1 last item in stack!");
                register_list.push(RegisterList::Range { lo, hi })
            }
        }
    }

    let rlist_string: String = register_list.iter().map(|rlist_type| {
        match rlist_type {
            RegisterList::Range { lo, hi } => format!("r{lo}-r{hi},"),
            RegisterList::Single(reg_num) => format!("r{reg_num},"),
        }
    }).collect();
    let rlist_string = rlist_string.as_str().trim_end_matches(",");

    match addressing_mode {
        IncrementBefore => format!("{mnemonic}{cond_str}ib r{rn}{write_back}, {{{rlist_string}}}{s_bit}"),
        IncrementAfter => format!("{mnemonic}{cond_str}ia r{rn}{write_back}, {{{rlist_string}}}{s_bit}"),
        DecrementBefore => format!("{mnemonic}{cond_str}db r{rn}{write_back}, {{{rlist_string}}}{s_bit}"),
        DecrementAfter => format!("{mnemonic}{cond_str}da r{rn}{write_back}, {{{rlist_string}}}{s_bit}"),

        EmptyStackDescend => format!("{mnemonic}{cond_str}ed sp{write_back}, {{{rlist_string}}}{s_bit}"),
        FullStackDescend => format!("{mnemonic}{cond_str}fd sp{write_back}, {{{rlist_string}}}{s_bit}"),
        EmptyStackAscend => format!("{mnemonic}{cond_str}ea sp{write_back}, {{{rlist_string}}}{s_bit}"),
        FullStackAscend => format!("{mnemonic}{cond_str}fa sp{write_back}, {{{rlist_string}}}{s_bit}"),
    }
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
