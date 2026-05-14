pub struct ArmInstructionInfo {
    pub instruction: ArmInstruction,
    pub condition: u8,
}

pub enum ArmInstruction {
    // branch instructions
    BX { rn: u8 },
    B { offset: u32 },
    BL { offset: u32 },

    // data processing instructions
    MOV { set_condition: bool, rd: u8 },
}

pub fn branch_and_exchange(opcode: u32) -> ArmInstructionInfo {
    let rn = (opcode & 0xF) as u8;
    ArmInstructionInfo {
        instruction: ArmInstruction::BX { rn },
        condition: (opcode >> 28) as u8,
    }
}

pub fn branch_and_link(opcode: u32) -> ArmInstructionInfo {
    let link = (opcode >> 8) & 1 == 1;
    let mut offset = (opcode & 0xFFFFFF) << 2;

    if opcode & (1 << 23) != 0 {
        offset |= 0xFC00_0000;
    }

    let instruction = if link {
        ArmInstruction::BL { offset }
    } else {
        ArmInstruction::B { offset }
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

pub fn data_processing(opcode: u32) -> ArmInstructionInfo {
    use crate::arm::arm_data_op::*;

    let data_op = ((opcode >> 21) & 0xF) as u8;
    let rn = ((opcode >> 16) & 0xF) as u8;
    let rd = ((opcode >> 12) & 0xF) as u8;
    let set_condition = (opcode >> 20) & 0x1 == 1;

    let instruction = match data_op {
        AND => todo!(),
        EOR => todo!(),
        SUB => todo!(),
        RSB => todo!(),
        ADD => todo!(),
        ADC => todo!(),
        SBC => todo!(),
        RSC => todo!(),
        TST => todo!(),
        TEQ => todo!(),
        CMP => todo!(),
        CMN => todo!(),
        ORR => todo!(),
        MOV => ArmInstruction::MOV { set_condition, rd },
        BIC => todo!(),
        MVN => todo!(),
        _ => panic!("Invalid data op! {data_op}"),
    };

    ArmInstructionInfo {
        instruction,
        condition: (opcode >> 28) as u8,
    }
}

pub fn undefined_arm(_opcode: u32) -> ArmInstructionInfo {
    todo!("Handle undefined instruction?")
}
