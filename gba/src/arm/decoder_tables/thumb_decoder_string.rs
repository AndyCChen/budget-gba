use crate::arm::decoder_tables::thumb_decoder::{AddSubOp, AluOperation, ThumbInstruction, ConditionBranchType};

impl ThumbInstruction {
    pub fn to_asm_string(&self, opcode_address: u32) -> String {
        #[rustfmt::skip]
        let asm_string = match self {
            ThumbInstruction::Lsl { shift, rs, rd } => format!("{:10} r{rd}, r{rs}, #{shift}", "lsl"),
            ThumbInstruction::Lsr { shift, rs, rd } => format!("{:10} r{rd}, r{rs}, #{shift}", "lsr"),
            ThumbInstruction::Asr { shift, rs, rd } => format!("{:10} r{rd}, r{rs}, #{shift}", "asr"),

            ThumbInstruction::Add { rd, rs, op } => match op {
                AddSubOp::Register(rn) =>    format!("{:10} r{rd}, r{rs}, r{rn}", "add"),
                AddSubOp::Immediate(expr) => format!("{:10} r{rd}, r{rs}, #{expr}", "add"),
            },
            ThumbInstruction::Sub { rd, rs, op } => match op {
                AddSubOp::Register(rn) =>    format!("{:10} r{rd}, r{rs}, r{rn}", "sub"),
                AddSubOp::Immediate(expr) => format!("{:10} r{rd}, r{rs}, #{expr}", "sub"),
            },

            ThumbInstruction::Mov { rd, offset } =>    format!("{:10} r{rd}, #{offset}", "mov"),
            ThumbInstruction::Cmp { rd, offset } =>    format!("{:10} r{rd}, #{offset}", "cmp"),
            ThumbInstruction::AddImm { rd, offset } => format!("{:10} r{rd}, #{offset}", "add"),
            ThumbInstruction::SubImm { rd, offset } => format!("{:10} r{rd}, #{offset}", "sub"),

            ThumbInstruction::AluOp { op, rs, rd } => {
                match op {
                    AluOperation::And => format!("{:10} r{rd}, r{rs}", "and"),
                    AluOperation::Eor => format!("{:10} r{rd}, r{rs}", "eor"),
                    AluOperation::Lsl => format!("{:10} r{rd}, r{rs}", "lsl"),
                    AluOperation::Lsr => format!("{:10} r{rd}, r{rs}", "lsr"),
                    AluOperation::Asr => format!("{:10} r{rd}, r{rs}", "asr"),
                    AluOperation::Adc => format!("{:10} r{rd}, r{rs}", "adc"),
                    AluOperation::Sbc => format!("{:10} r{rd}, r{rs}", "sbc"),
                    AluOperation::Ror => format!("{:10} r{rd}, r{rs}", "ror"),
                    AluOperation::Tst => format!("{:10} r{rd}, r{rs}", "tst"),
                    AluOperation::Neg => format!("{:10} r{rd}, r{rs}", "neg"),
                    AluOperation::Cmp => format!("{:10} r{rd}, r{rs}", "cmp"),
                    AluOperation::Cmn => format!("{:10} r{rd}, r{rs}", "cmn"),
                    AluOperation::Orr => format!("{:10} r{rd}, r{rs}", "orr"),
                    AluOperation::Mul => format!("{:10} r{rd}, r{rs}", "mul"),
                    AluOperation::Bic => format!("{:10} r{rd}, r{rs}", "bic"),
                    AluOperation::Mvn => format!("{:10} r{rd}, r{rs}", "mvn"),
                }
            }

            ThumbInstruction::AddHi { rd, rs } => format!("{:10} r{rd}, r{rs}", "add"),
            ThumbInstruction::CmpHi { rd, rs } => format!("{:10} r{rd}, r{rs}", "cmp"),
            ThumbInstruction::MovHi { rd, rs } => format!("{:10} r{rd}, r{rs}", "mov"),
            ThumbInstruction::BxHi { rs } =>      format!("{:10} r{rs}", "bx"),

            ThumbInstruction::PcRelativeLoad { rd, offset } => format!("{:10} r{rd}, [PC, #{offset}]", "ldr"),

            ThumbInstruction::LdrRegister { is_byte, rd, rb, ro } => format!("{:10} r{rd}, [r{rb}, r{ro}]", format!("ldr{}", if *is_byte {"b"} else {""})),
            ThumbInstruction::StrRegister { is_byte, rd, rb, ro } => format!("{:10} r{rd}, [r{rb}, r{ro}]", format!("str{}", if *is_byte {"b"} else {""})),

            ThumbInstruction::LoadSignedByteHalfword { is_byte, is_signed, rd, rb, ro } => {
                match (is_byte, is_signed) {
                    (true, true) =>   format!("{:10} r{rd}, [r{rb}, r{ro}]", "ldsb"),
                    (false, true) =>  format!("{:10} r{rd}, [r{rb}, r{ro}]", "ldsh"),
                    (false, false) => format!("{:10} r{rd}, [r{rb}, r{ro}]", "ldrh"),
                    (true, false) =>  panic!("Invalid op for load!"),
                }
            }
            ThumbInstruction::StoreHalfword { rd, rb, ro } => format!("{:10} r{rd}, [r{rb}, r{ro}]", "strh"),

            ThumbInstruction::LoadImm { is_byte, rd, rb, offset } =>  format!("{:10} r{rd}, [r{rb}, #{offset}]", format!("ldr{}", if *is_byte {"b"} else {""})),
            ThumbInstruction::StoreImm { is_byte, rd, rb, offset } => format!("{:10} r{rd}, [r{rb}, #{offset}]", format!("str{}", if *is_byte {"b"} else {""})),

            ThumbInstruction::LoadOffsetHalfword { rd, rb, offset } => format!("{:10} r{rd}, [r{rb}, #{offset}]", "ldrh"),
            ThumbInstruction::StoreOffsetHalfword { rd, rb, offset } => format!("{:10} r{rd}, [r{rb}, #{offset}]", "strh"),

            ThumbInstruction::LoadSpRelative { rd, offset } => format!("{:10} r{rd}, [SP, #{offset}]", "ldr"),
            ThumbInstruction::StoreSpRelative { rd, offset } => format!("{:10} r{rd}, [SP, #{offset}]", "str"),

            ThumbInstruction::PcSpLoad { is_stack_pointer, rd, offset } => format!("{:10} r{rd}, {}, #{offset}", "add", if *is_stack_pointer {"SP"} else {"PC"}),

            ThumbInstruction::SpAddOffset { offset } => format!("{:10} SP, #{offset}", "add"),

            ThumbInstruction::Push { transfer_sp_pc, rlist } => format!("{:10} {{{}{}}}", "push", format_rlist(*rlist), if *transfer_sp_pc {", LR"} else {""}),
            ThumbInstruction::Pop { transfer_sp_pc, rlist } => format!("{:10} {{{}{}}}", "pop", format_rlist(*rlist), if *transfer_sp_pc {", PC"} else {""}),

            ThumbInstruction::Ldm { rb, rlist } => format!("{:10} r{rb}! {{{}}}", "ldmia", format_rlist(*rlist)),
            ThumbInstruction::Stm { rb, rlist } => format!("{:10} r{rb}! {{{}}}", "stmia", format_rlist(*rlist)),

            ThumbInstruction::ConditionalBranch { cond, offset } => format!("{:10} 0x{:08X}", format!("b{}", format_branch_condition(*cond)), opcode_address.wrapping_add(*offset)),

            ThumbInstruction::Swi { comment_field } => format!("{:10} 0x{comment_field:02X}", "swi"),

            ThumbInstruction::UnconditionalBranch { offset } => format!("{:10} 0x{:08X}", "b", opcode_address.wrapping_add(*offset)),

            ThumbInstruction::LongBranchLinkFirst { offset } => format!("{:10} hi 0x{offset:08X}", "bl"),
            ThumbInstruction::LongBranchLinkSecond { offset } => format!("{:10} lo 0x{offset:08X}", "bl"),

            ThumbInstruction::Und { opcode } => format!("{:10} 0x{opcode:08X}", "undef"),
        };

        asm_string
    }
}

fn format_branch_condition(condition: ConditionBranchType) -> &'static str {
    match condition {
        ConditionBranchType::EQ => "eq",
        ConditionBranchType::NE => "ne",
        ConditionBranchType::CS => "cs",
        ConditionBranchType::CC => "cc",
        ConditionBranchType::MI => "mi",
        ConditionBranchType::PL => "pl",
        ConditionBranchType::VS => "vs",
        ConditionBranchType::VC => "vc",
        ConditionBranchType::HI => "hi",
        ConditionBranchType::LS => "ls",
        ConditionBranchType::GE => "ge",
        ConditionBranchType::LT => "lt",
        ConditionBranchType::GT => "gt",
        ConditionBranchType::LE => "le",
    }
}

fn format_rlist(rlist: u8) -> String {
    enum RegisterList {
        Range{ lo: u8, hi: u8 },
        Single(u8)
    }

    let mut register_list = Vec::with_capacity(8);
    let mut stack = Vec::with_capacity(8);

    for b in 0..8 {
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

    rlist_string.as_str().trim_end_matches(",").into()
}

#[cfg(test)]
mod test {
    use crate::arm::decoder_tables::decode_thumb;

    #[test]
    fn test_decode() {
        let opcode = (0b11100 << 11) | 8;
        println!("{}", decode_thumb(opcode).to_asm_string(4));
    }
}
