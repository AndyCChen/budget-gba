use crate::arm::decoder_tables::thumb_decoder::{AddSubOp, AluOperation, ThumbInstruction, ConditionBranchType};

impl ThumbInstruction {
    pub fn to_asm_string(&self, opcode_address: u32) -> String {
        #[rustfmt::skip]
        let asm_string = match self {
            ThumbInstruction::Lsl { shift, rs, rd } => format!("lsl r{rd}, r{rs}, #{shift}"),
            ThumbInstruction::Lsr { shift, rs, rd } => format!("lsr r{rd}, r{rs}, #{shift}"),
            ThumbInstruction::Asr { shift, rs, rd } => format!("asr r{rd}, r{rs}, #{shift}"),

            ThumbInstruction::Add { rd, rs, op } => match op {
                AddSubOp::Register(rn) =>    format!("add r{rd}, r{rs}, r{rn}"),
                AddSubOp::Immediate(expr) => format!("add r{rd}, r{rs}, #{expr}"),
            },
            ThumbInstruction::Sub { rd, rs, op } => match op {
                AddSubOp::Register(rn) =>    format!("sub r{rd}, r{rs}, r{rn}"),
                AddSubOp::Immediate(expr) => format!("sub r{rd}, r{rs}, #{expr}"),
            },

            ThumbInstruction::Mov { rd, offset } =>    format!("mov r{rd}, #{offset}"),
            ThumbInstruction::Cmp { rd, offset } =>    format!("cmp r{rd}, #{offset}"),
            ThumbInstruction::AddImm { rd, offset } => format!("add r{rd}, #{offset}"),
            ThumbInstruction::SubImm { rd, offset } => format!("sub r{rd}, #{offset}"),

            ThumbInstruction::AluOp { op, rs, rd } => {
                match op {
                    AluOperation::And => format!("and r{rd}, r{rs}"),
                    AluOperation::Eor => format!("eor r{rd}, r{rs}"),
                    AluOperation::Lsl => format!("lsl r{rd}, r{rs}"),
                    AluOperation::Lsr => format!("lsr r{rd}, r{rs}"),
                    AluOperation::Asr => format!("asr r{rd}, r{rs}"),
                    AluOperation::Adc => format!("adc r{rd}, r{rs}"),
                    AluOperation::Sbc => format!("sbc r{rd}, r{rs}"),
                    AluOperation::Ror => format!("ror r{rd}, r{rs}"),
                    AluOperation::Tst => format!("tst r{rd}, r{rs}"),
                    AluOperation::Neg => format!("neg r{rd}, r{rs}"),
                    AluOperation::Cmp => format!("cmp r{rd}, r{rs}"),
                    AluOperation::Cmn => format!("cmn r{rd}, r{rs}"),
                    AluOperation::Orr => format!("orr r{rd}, r{rs}"),
                    AluOperation::Mul => format!("mul r{rd}, r{rs}"),
                    AluOperation::Bic => format!("bic r{rd}, r{rs}"),
                    AluOperation::Mvn => format!("mvn r{rd}, r{rs}"),
                }
            }

            ThumbInstruction::AddHi { rd, rs } => format!("add r{rd}, r{rs}"),
            ThumbInstruction::CmpHi { rd, rs } => format!("cmp r{rd}, r{rs}"),
            ThumbInstruction::MovHi { rd, rs } => format!("mov r{rd}, r{rs}"),
            ThumbInstruction::BxHi { rs } =>      format!("bx r{rs}"),

            ThumbInstruction::PcRelativeLoad { rd, offset } => format!("ldr r{rd}, [PC, #{offset}]"),

            ThumbInstruction::LdrRegister { is_byte, rd, rb, ro } => format!("ldr{} r{rd}, [r{rb}, r{ro}]", if *is_byte {"b"} else {""}),
            ThumbInstruction::StrRegister { is_byte, rd, rb, ro } => format!("str{} r{rd}, [r{rb}, r{ro}]", if *is_byte {"b"} else {""}),

            ThumbInstruction::LoadSignedByteHalfword { is_byte, is_signed, rd, rb, ro } => {
                match (is_byte, is_signed) {
                    (true, true) =>   format!("ldsb r{rd}, [r{rb}, r{ro}]"),
                    (false, true) =>  format!("ldsh r{rd}, [r{rb}, r{ro}]"),
                    (false, false) => format!("ldrh r{rd}, [r{rb}, r{ro}]"),
                    (true, false) =>  panic!("Invalid op for load!"),
                }
            }
            ThumbInstruction::StoreHalfword { rd, rb, ro } => format!("strh r{rd}, [r{rb}, r{ro}]"),

            ThumbInstruction::LoadImm { is_byte, rd, rb, offset } =>  format!("ldr{} r{rd}, [r{rb}, #{offset}]", if *is_byte {"b"} else {""}),
            ThumbInstruction::StoreImm { is_byte, rd, rb, offset } => format!("str{} r{rd}, [r{rb}, #{offset}]", if *is_byte {"b"} else {""}),

            ThumbInstruction::LoadOffsetHalfword { rd, rb, offset } => format!("ldrh r{rd}, [r{rb}, #{offset}]"),
            ThumbInstruction::StoreOffsetHalfword { rd, rb, offset } => format!("strh r{rd}, [r{rb}, #{offset}]"),

            ThumbInstruction::LoadSpRelative { rd, offset } => format!("ldr r{rd}, [SP, #{offset}]"),
            ThumbInstruction::StoreSpRelative { rd, offset } => format!("str r{rd}, [SP, #{offset}]"),

            ThumbInstruction::PcSpLoad { is_stack_pointer, rd, offset } => format!("add r{rd}, {}, #{offset}", if *is_stack_pointer {"SP"} else {"PC"}),

            ThumbInstruction::SpAddOffset { offset } => format!("add SP, #{offset}"),

            ThumbInstruction::Push { transfer_sp_pc, rlist } => format!("push {{{}{}}}", format_rlist(*rlist), if *transfer_sp_pc {", LR"} else {""}),
            ThumbInstruction::Pop { transfer_sp_pc, rlist } => format!("pop {{{}{}}}", format_rlist(*rlist), if *transfer_sp_pc {", PC"} else {""}),

            ThumbInstruction::Ldm { rb, rlist } => format!("ldmia r{rb}! {{{}}}", format_rlist(*rlist)),
            ThumbInstruction::Stm { rb, rlist } => format!("stmia r{rb}! {{{}}}", format_rlist(*rlist)),

            ThumbInstruction::ConditionalBranch { cond, offset } => format!("b{} 0x{:08X}", format_branch_condition(*cond), opcode_address.wrapping_add(*offset)),

            ThumbInstruction::Swi { comment_field } => format!("swi 0x{comment_field:02X}"),

            ThumbInstruction::UnconditionalBranch { offset } => format!("b 0x{:08X}", opcode_address.wrapping_add(*offset)),

            ThumbInstruction::LongBranchLinkFirst { offset } => format!("bl hi 0x{offset:08X}"),
            ThumbInstruction::LongBranchLinkSecond { offset } => format!("bl lo 0x{offset:08X}"),

            ThumbInstruction::Und { opcode } => format!("undefined 0x{opcode:08X}"),
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
