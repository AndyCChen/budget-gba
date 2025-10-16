use std::num::Wrapping;

use crate::arm::constants::{access_code, kind_code};
use crate::arm::instruction_lut::ARM_TABLE;
use crate::arm::json_test_states::*;
use bitfield_struct::bitfield;

pub struct Arm7tdmi {
    transaction_index: usize,
    pub registers: GeneralRegisters,
    pub status: StatusRegisters,
    pub pipeline: [Option<u32>; 3], // [execute, decode, fetch]
    pub pipeline_state: u8,
    pub transactions: Vec<Transactions>,
}

impl Arm7tdmi {
    pub fn new(input_state: &InputStates) -> Self {
        Self {
            transaction_index: 1,
            registers: GeneralRegisters {
                r0: input_state.initial.R[0],
                r1: input_state.initial.R[1],
                r2: input_state.initial.R[2],
                r3: input_state.initial.R[3],
                r4: input_state.initial.R[4],
                r5: input_state.initial.R[5],
                r6: input_state.initial.R[6],
                r7: input_state.initial.R[7],

                r8: input_state.initial.R[8],
                r8_fiq: input_state.initial.R_fiq[0],

                r9: input_state.initial.R[9],
                r9_fiq: input_state.initial.R_fiq[1],

                r10: input_state.initial.R[10],
                r10_fiq: input_state.initial.R_fiq[2],

                r11: input_state.initial.R[11],
                r11_fiq: input_state.initial.R_fiq[3],

                r12: input_state.initial.R[12],
                r12_fiq: input_state.initial.R_fiq[4],

                r13: input_state.initial.R[13],
                r13_fiq: input_state.initial.R_fiq[5],
                r13_svc: input_state.initial.R_svc[0],
                r13_abt: input_state.initial.R_abt[0],
                r13_irq: input_state.initial.R_irq[0],
                r13_und: input_state.initial.R_und[0],

                r14: input_state.initial.R[14],
                r14_fiq: input_state.initial.R_fiq[6],
                r14_svc: input_state.initial.R_svc[1],
                r14_abt: input_state.initial.R_abt[1],
                r14_irq: input_state.initial.R_irq[1],
                r14_und: input_state.initial.R_und[1],

                r15: Wrapping(input_state.initial.R[15]),
            },
            status: StatusRegisters {
                cpsr: StatusRegister::from_bits(input_state.initial.CPSR),
                spsr_fiq: StatusRegister::from_bits(input_state.initial.SPSR[0]),
                spsr_svc: StatusRegister::from_bits(input_state.initial.SPSR[1]),
                spsr_abt: StatusRegister::from_bits(input_state.initial.SPSR[2]),
                spsr_irq: StatusRegister::from_bits(input_state.initial.SPSR[3]),
                spsr_und: StatusRegister::from_bits(input_state.initial.SPSR[4]),
            },
            pipeline: [
                Some(input_state.initial.pipeline[0]),
                Some(input_state.initial.pipeline[1]),
                None,
            ],
            pipeline_state: input_state.initial.access,
            transactions: input_state.transactions.clone(),
        }
    }

    pub fn run(&mut self) {
        if let Some(opcode) = self.pipeline[0] {
            // execute in thumb mode
            if self.status.cpsr.t() {
                panic!("Thumb mode not implemented!");
                //self.pipeline_prefetch(true);
            }
            // execute in arm mode
            else {
                self.pipeline_prefetch(false);
                if !self.condition_check((opcode >> 28) as u8) {
                    self.registers.r15 += 4;
                    return;
                }

                let arm_table_hash: usize = (((opcode & 0x0FF00000) >> 16)
                    | ((opcode & 0xF0) >> 4))
                    .try_into()
                    .unwrap();
                ARM_TABLE[arm_table_hash](self, opcode);
            }
        }
    }

    /// Retrieve register in arm mode
    pub fn get_register_arm(&self, register_id: u32) -> u32 {
        match (register_id, self.status.cpsr.mode_bits()) {
            (0, _) => self.registers.r0,
            (1, _) => self.registers.r1,
            (2, _) => self.registers.r2,
            (3, _) => self.registers.r3,
            (4, _) => self.registers.r4,
            (5, _) => self.registers.r5,
            (6, _) => self.registers.r6,
            (7, _) => self.registers.r7,

            (8, Mode::Fiq) => self.registers.r8_fiq,
            (8, _) => self.registers.r8,

            (9, Mode::Fiq) => self.registers.r9_fiq,
            (9, _) => self.registers.r9,

            (10, Mode::Fiq) => self.registers.r10_fiq,
            (10, _) => self.registers.r10,

            (11, Mode::Fiq) => self.registers.r11_fiq,
            (11, _) => self.registers.r11,

            (12, Mode::Fiq) => self.registers.r12_fiq,
            (12, _) => self.registers.r12,

            (13, Mode::User | Mode::System) => self.registers.r13,
            (13, Mode::Fiq) => self.registers.r13_fiq,
            (13, Mode::Supervisor) => self.registers.r13_svc,
            (13, Mode::Abort) => self.registers.r13_abt,
            (13, Mode::Irq) => self.registers.r13_irq,
            (13, Mode::Undefined) => self.registers.r13_und,

            (14, Mode::User | Mode::System) => self.registers.r14,
            (14, Mode::Fiq) => self.registers.r14_fiq,
            (14, Mode::Supervisor) => self.registers.r14_svc,
            (14, Mode::Abort) => self.registers.r14_abt,
            (14, Mode::Irq) => self.registers.r14_irq,
            (14, Mode::Undefined) => self.registers.r14_und,

            (15, _) => self.registers.r15.0,

            _ => panic!(
                "Register id must be in range 1-15! {register_id} {:?}",
                self.status.cpsr.mode_bits()
            ),
        }
    }

    pub fn set_register_arm(&mut self, register_id: u32, value: u32) {
        match (register_id, &self.status.cpsr.mode_bits()) {
            (0, _) => self.registers.r0 = value,
            (1, _) => self.registers.r1 = value,
            (2, _) => self.registers.r2 = value,
            (3, _) => self.registers.r3 = value,
            (4, _) => self.registers.r4 = value,
            (5, _) => self.registers.r5 = value,
            (6, _) => self.registers.r6 = value,
            (7, _) => self.registers.r7 = value,

            (8, Mode::Fiq) => self.registers.r8_fiq = value,
            (8, _) => self.registers.r8 = value,

            (9, Mode::Fiq) => self.registers.r9_fiq = value,
            (9, _) => self.registers.r9 = value,

            (10, Mode::Fiq) => self.registers.r10_fiq = value,
            (10, _) => self.registers.r10 = value,

            (11, Mode::Fiq) => self.registers.r11_fiq = value,
            (11, _) => self.registers.r11 = value,

            (12, Mode::Fiq) => self.registers.r12_fiq = value,
            (12, _) => self.registers.r12 = value,

            (13, Mode::User | Mode::System) => self.registers.r13 = value,
            (13, Mode::Fiq) => self.registers.r13_fiq = value,
            (13, Mode::Supervisor) => self.registers.r13_svc = value,
            (13, Mode::Abort) => self.registers.r13_abt = value,
            (13, Mode::Irq) => self.registers.r13_irq = value,
            (13, Mode::Undefined) => self.registers.r13_und = value,

            (14, Mode::User | Mode::System) => self.registers.r14 = value,
            (14, Mode::Fiq) => self.registers.r14_fiq = value,
            (14, Mode::Supervisor) => self.registers.r14_svc = value,
            (14, Mode::Abort) => self.registers.r14_abt = value,
            (14, Mode::Irq) => self.registers.r14_irq = value,
            (14, Mode::Undefined) => self.registers.r14_und = value,

            (15, _) => self.registers.r15 = Wrapping(value),

            _ => panic!(
                "Register id must be in range 1-15! {register_id} {:?}",
                self.status.cpsr.mode_bits()
            ),
        }
    }

    // retrieve banked spsr from current corresponding mode.
    // if mode is user/system, returns the cpsr
    pub fn get_spsr(&self) -> u32 {
        match self.status.cpsr.mode_bits() {
            Mode::User | Mode::System => self.status.cpsr.into_bits(),
            Mode::Fiq => self.status.spsr_fiq.into_bits(),
            Mode::Irq => self.status.spsr_irq.into_bits(),
            Mode::Supervisor => self.status.spsr_svc.into_bits(),
            Mode::Abort => self.status.spsr_abt.into_bits(),
            Mode::Undefined => self.status.spsr_und.into_bits(),
        }
    }

    // set banked spsr of the current corresponding mode.
    // if mode is user/system, sets the cpsr
    pub fn set_spsr(&mut self, value: u32) {
        match self.status.cpsr.mode_bits() {
            Mode::User | Mode::System => (),
            Mode::Fiq => self.status.spsr_fiq = StatusRegister::from_bits(value),
            Mode::Irq => self.status.spsr_irq = StatusRegister::from_bits(value),
            Mode::Supervisor => self.status.spsr_svc = StatusRegister::from_bits(value),
            Mode::Abort => self.status.spsr_abt = StatusRegister::from_bits(value),
            Mode::Undefined => self.status.spsr_und = StatusRegister::from_bits(value),
        };
    }

    fn pipeline_read_word(&mut self, address: u32, access: u8) -> u32 {
        let address = address & !3; // align 4 byte boundary
        let data = self.transactions[self.transaction_index - 1].clone();
        self.transaction_next();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(data.size, 4, "mismatch size!");
        assert_eq!(
            data.kind,
            kind_code::INSTRUCTION_READ,
            "mismatch kind code!"
        );

        data.data
    }

    pub fn read_rotate_word(&mut self, address: u32, access: u8) -> u32 {
        let data = self.transactions[self.transaction_index - 1].clone();
        self.transaction_next();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(data.size, 4, "mismatch size!");
        assert_eq!(data.kind, kind_code::GENERAL_READ, "mismatch kind code!");

        data.data.rotate_right((address & 3) * 8)
    }

    fn pipeline_read_halfword(&mut self, address: u32, access: u8) -> u16 {
        let address = address & !1;
        let data = self.transactions[self.transaction_index - 1].clone();
        self.transaction_next();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.size, 2, "mismatch size!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(
            data.kind,
            kind_code::INSTRUCTION_READ,
            "mismatch kind code!"
        );

        data.data as u16
    }

    pub fn read_halfword(&mut self, address: u32, access: u8) -> u16 {
        let data = self.transactions[self.transaction_index - 1].clone();
        self.transaction_next();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(data.size, 2, "mismatch size!");
        assert_eq!(data.kind, kind_code::GENERAL_READ, "mismatch kind code!");

        data.data as u16
    }

    pub fn write_halfword(&mut self, address: u32, value: u16, access: u8) {
        let data = self.transactions[self.transaction_index - 1].clone();
        self.transaction_next();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(data.size, 2, "mismatch size!");
        assert_eq!(data.kind, kind_code::WRITE, "mismatch kind code!");
        assert_eq!(data.data, Into::<u32>::into(value), "mismatch write value!");
    }

    pub fn write_word(&mut self, address: u32, value: u32, access: u8) {
        let data = self.transactions[self.transaction_index - 1].clone();
        self.transaction_next();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(data.size, 4, "mismatch size!");
        assert_eq!(data.kind, kind_code::WRITE, "mismatch kind code!");
        assert_eq!(data.data, value, "mismatch write value!");
    }

    pub fn read_byte(&mut self, address: u32, access: u8) -> u8 {
        let data = self.transactions[self.transaction_index - 1].clone();
        self.transaction_next();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(data.size, 1, "mismatch size!");
        assert_eq!(data.kind, kind_code::GENERAL_READ, "mismatch kind code!");

        data.data as u8
    }

    pub fn write_byte(&mut self, address: u32, value: u8, access: u8) {
        let data = self.transactions[self.transaction_index - 1].clone();
        self.transaction_next();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(data.size, 1, "mismatch size!");
        assert_eq!(data.kind, kind_code::WRITE, "mismatch kind code!");
        assert_eq!(data.data, Into::<u32>::into(value), "mismatch write value!");
    }

    fn transaction_next(&mut self) {
        self.transaction_index += 1;
    }

    /// Flush and refills the pipeline for arm mode
    pub fn pipeline_refill_arm(&mut self) {
        self.pipeline[0] = Some(self.pipeline_read_word(
            self.registers.r15.0,
            access_code::CODE | access_code::NONSEQUENTIAL,
        ));
        self.pipeline[1] = Some(self.pipeline_read_word(
            self.registers.r15.0.wrapping_add(4),
            access_code::CODE | access_code::SEQUENTIAL,
        ));
        self.registers.r15 += 8;
    }

    /// Flush and refills the pipeline for thumb mode
    pub fn pipeline_refill_thumb(&mut self) {
        self.pipeline[0] = Some(
            self.pipeline_read_halfword(
                self.registers.r15.0,
                access_code::CODE | access_code::NONSEQUENTIAL,
            )
            .into(),
        );
        self.pipeline[1] = Some(
            self.pipeline_read_halfword(
                self.registers.r15.0.wrapping_add(2),
                access_code::CODE | access_code::SEQUENTIAL,
            )
            .into(),
        );
        self.registers.r15 += 4;
    }

    /// fetch opcode and push into pipeline
    fn pipeline_prefetch(&mut self, is_thumb: bool) {
        if is_thumb {
            self.registers.r15 &= !0x1;
            panic!("no thumb for pipeline prefetch yet!");
        } else {
            self.registers.r15 &= !0x3;
            self.pipeline[2] =
                Some(self.pipeline_read_word(self.registers.r15.0, self.pipeline_state));
        }
        self.pipeline.copy_within(1..3, 0);
    }

    /// Checks 4-bit condition code that determines if instruction should be executed or skipped.
    /// ### Returns
    /// True if opcode can be executed, else False
    fn condition_check(&self, cond: u8) -> bool {
        use crate::arm::constants::arm_condition_code::*;

        match cond {
            EQ => self.status.cpsr.z(),
            NE => !self.status.cpsr.z(),
            CS => self.status.cpsr.c(),
            CC => !self.status.cpsr.c(),
            MI => self.status.cpsr.n(),
            PL => !self.status.cpsr.n(),
            VS => self.status.cpsr.v(),
            VC => !self.status.cpsr.v(),
            HI => self.status.cpsr.c() && !self.status.cpsr.z(),
            LS => !self.status.cpsr.c() || self.status.cpsr.z(),
            GE => self.status.cpsr.n() == self.status.cpsr.v(),
            LT => self.status.cpsr.n() != self.status.cpsr.v(),
            GT => !self.status.cpsr.z() && (self.status.cpsr.n() == self.status.cpsr.v()),
            LE => self.status.cpsr.z() || (self.status.cpsr.n() != self.status.cpsr.v()),
            AL => true,
            _ => {
                println!("Undefined condition: {cond}");
                false
            }
        }
    }
}

pub struct StatusRegisters {
    pub cpsr: StatusRegister,
    pub spsr_fiq: StatusRegister,
    pub spsr_svc: StatusRegister,
    pub spsr_abt: StatusRegister,
    pub spsr_irq: StatusRegister,
    pub spsr_und: StatusRegister,
}

#[derive(Default, Debug)]
pub struct GeneralRegisters {
    pub r0: u32,
    pub r1: u32,
    pub r2: u32,
    pub r3: u32,
    pub r4: u32,
    pub r5: u32,
    pub r6: u32,
    pub r7: u32,

    pub r8: u32,
    pub r8_fiq: u32,

    pub r9: u32,
    pub r9_fiq: u32,

    pub r10: u32,
    pub r10_fiq: u32,

    pub r11: u32,
    pub r11_fiq: u32,

    pub r12: u32,
    pub r12_fiq: u32,

    pub r13: u32, // stack pointer (sp)
    pub r13_fiq: u32,
    pub r13_svc: u32,
    pub r13_abt: u32,
    pub r13_irq: u32,
    pub r13_und: u32,

    pub r14: u32, // link registers (lr)
    pub r14_fiq: u32,
    pub r14_svc: u32,
    pub r14_abt: u32,
    pub r14_irq: u32,
    pub r14_und: u32,

    pub r15: Wrapping<u32>, // program counter (pc)
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Mode {
    User = 0b10000,
    Fiq = 0b10001,
    Irq = 0b10010,
    Supervisor = 0b10011,
    Abort = 0b10111,
    Undefined = 0b11011,
    System = 0b11111,
}

impl Mode {
    const fn into_bits(self) -> u8 {
        self as u8
    }

    const fn from_bits(value: u8) -> Self {
        match value {
            0b10000 => Mode::User,
            0b10001 => Mode::Fiq,
            0b10010 => Mode::Irq,
            0b10011 => Mode::Supervisor,
            0b10111 => Mode::Abort,
            0b11011 => Mode::Undefined,
            0b11111 => Mode::System,
            _ => panic!("invalid mode"),
        }
    }
}

#[bitfield(u32)]
pub struct StatusRegister {
    #[bits(5, default = Mode::User, from = Mode::from_bits)]
    pub mode_bits: Mode,

    // 0: arm mode, 1: thumb mode,
    pub t: bool,

    // 0: enable fiq, 1: disable fiq
    pub f: bool,

    // 0: enable irq, 1: disable irq
    pub i: bool,

    #[bits(20)]
    // reserved
    __: u32,

    // overflow
    pub v: bool,

    // carry flag
    pub c: bool,

    // zero flag
    pub z: bool,

    // negative flag
    pub n: bool,
}

#[cfg(test)]
#[rustfmt::skip]
mod arm7tdmi_tests {
    use super::*;
    use std::fs;
    use std::path::Path;

    fn load_test<P: AsRef<Path>>(
        path: P,
        check_state: fn(cpu: &Arm7tdmi, input_state: &InputStates, test_num: usize),
        skip: usize,
    ) {
        let Ok(data) = fs::read_to_string(path) else {
            panic!("Failed to load test file!");
        };

        let items: Vec<InputStates> = serde_json::from_str(&data).unwrap();
        let it = items.into_iter().enumerate().skip(skip);

        for (count, item) in it {
            let mut cpu = Arm7tdmi::new(&item);
            cpu.run();
            check_state(&cpu, &item, count);
        }
    }

    // ignore checking carry flag, useful for checking muliply instruction as the carry flag result is not emulated
    fn verify_state_no_carry(cpu: &Arm7tdmi, input_state: &InputStates, test_num: usize) {
        let mask = 0xDFFF_FFFF;
        assert_eq!(cpu.status.cpsr.into_bits() & mask, input_state.r#final.CPSR & mask, "{input_state:#?} cspr, test: {test_num}");
        verify_state_core(cpu, input_state, test_num);
    }

    fn verify_state_no_carry_overflow(cpu: &Arm7tdmi, input_state: &InputStates, test_num: usize) {
        let mask = 0xCFFF_FFFF;
        assert_eq!(cpu.status.cpsr.into_bits() & mask, input_state.r#final.CPSR & mask, "{input_state:#?} cspr, test: {test_num}");
        verify_state_core(cpu, input_state, test_num);
    }

    fn verify_state(cpu: &Arm7tdmi, input_state: &InputStates, test_num: usize) {
        assert_eq!(cpu.status.cpsr.into_bits(), input_state.r#final.CPSR, "{input_state:#?} cspr, test: {test_num}");
        verify_state_core(cpu, input_state, test_num);
    }
    
    fn verify_state_core(cpu: &Arm7tdmi, input_state: &InputStates, test_num: usize) {
        let final_state = &input_state.r#final;

        assert_eq!(cpu.status.spsr_fiq.into_bits(), final_state.SPSR[0], "{input_state:#?} spsr_fiq, test: {test_num}");
        assert_eq!(cpu.status.spsr_svc.into_bits(), final_state.SPSR[1], "{input_state:#?} spsr_svc, test: {test_num}");
        assert_eq!(cpu.status.spsr_abt.into_bits(), final_state.SPSR[2], "{input_state:#?} spsr_abt, test: {test_num}");
        assert_eq!(cpu.status.spsr_irq.into_bits(), final_state.SPSR[3], "{input_state:#?} spsr_irq, test: {test_num}");
        assert_eq!(cpu.status.spsr_und.into_bits(), final_state.SPSR[4], "{input_state:#?} spsr_und, test: {test_num}");

        assert_eq!(cpu.registers.r0, final_state.R[0], "{input_state:#?} r0, test: {test_num}");
        assert_eq!(cpu.registers.r1, final_state.R[1], "{input_state:#?} r1, test: {test_num}");
        assert_eq!(cpu.registers.r2, final_state.R[2], "{input_state:#?} r2, test: {test_num}");
        assert_eq!(cpu.registers.r3, final_state.R[3], "{input_state:#?} r3, test: {test_num}");
        assert_eq!(cpu.registers.r4, final_state.R[4], "{input_state:#?} r4, test: {test_num}");
        assert_eq!(cpu.registers.r5, final_state.R[5], "{input_state:#?} r5, test: {test_num}");
        assert_eq!(cpu.registers.r6, final_state.R[6], "{input_state:#?} r6, test: {test_num}");
        assert_eq!(cpu.registers.r7, final_state.R[7], "{input_state:#?} r7, test: {test_num}");
        assert_eq!(cpu.registers.r8, final_state.R[8], "{input_state:#?} r8, test: {test_num}");
        assert_eq!(cpu.registers.r9, final_state.R[9], "{input_state:#?} r9, test: {test_num}");
        assert_eq!(cpu.registers.r10, final_state.R[10], "{input_state:#?} r10, test: {test_num}");
        assert_eq!(cpu.registers.r11, final_state.R[11], "{input_state:#?} r11, test: {test_num}");
        assert_eq!(cpu.registers.r12, final_state.R[12], "{input_state:#?} r12, test: {test_num}");
        assert_eq!(cpu.registers.r13, final_state.R[13], "{input_state:#?} r13, test: {test_num}");
        assert_eq!(cpu.registers.r14, final_state.R[14], "{input_state:#?} r14, test: {test_num}");
        assert_eq!(cpu.registers.r15.0, final_state.R[15], "{input_state:#?} r15, test: {test_num}");

        assert_eq!(cpu.registers.r8_fiq, final_state.R_fiq[0], "{input_state:#?} r8_fiq, test: {test_num}");
        assert_eq!(cpu.registers.r9_fiq, final_state.R_fiq[1], "{input_state:#?} r9_fiq, test: {test_num}");
        assert_eq!(cpu.registers.r10_fiq, final_state.R_fiq[2], "{input_state:#?} r10_fiq, test: {test_num}");
        assert_eq!(cpu.registers.r11_fiq, final_state.R_fiq[3], "{input_state:#?} r11_fiq, test: {test_num}");
        assert_eq!(cpu.registers.r12_fiq, final_state.R_fiq[4], "{input_state:#?} r12_fiq, test: {test_num}");
        assert_eq!(cpu.registers.r13_fiq, final_state.R_fiq[5], "{input_state:#?} r13_fiq, test: {test_num}");
        assert_eq!(cpu.registers.r14_fiq, final_state.R_fiq[6], "{input_state:#?} r14_fiq, test: {test_num}");

        assert_eq!(cpu.registers.r13_svc, final_state.R_svc[0], "{input_state:#?} r13_svc, test: {test_num}");
        assert_eq!(cpu.registers.r14_svc, final_state.R_svc[1], "{input_state:#?} r14_svc, test: {test_num}");

        assert_eq!(cpu.registers.r13_abt, final_state.R_abt[0], "{input_state:#?} r13_abt, test: {test_num}");
        assert_eq!(cpu.registers.r14_abt, final_state.R_abt[1], "{input_state:#?} r14_abt, test: {test_num}");

        assert_eq!(cpu.registers.r13_irq, final_state.R_irq[0], "{input_state:#?} r13_irq, test: {test_num}");
        assert_eq!(cpu.registers.r14_irq, final_state.R_irq[1], "{input_state:#?} r14_irq, test: {test_num}");

        assert_eq!(cpu.registers.r13_und, final_state.R_und[0], "{input_state:#?} r13_und, test: {test_num}");
        assert_eq!(cpu.registers.r14_und, final_state.R_und[1], "{input_state:#?} r14_und, test: {test_num}");

        assert_eq!(cpu.pipeline[0], Some(final_state.pipeline[0]), "{input_state:#?} pipeline_0, test: {test_num}");
        assert_eq!(cpu.pipeline[1], Some(final_state.pipeline[1]), "{input_state:#?} pipeline_1, test: {test_num}");
    }

    #[test]
    fn test_arm_branch_and_exchange() {
        load_test("ARM7TDMI/v1/arm_bx.json", verify_state, 0);
    }

    #[test]
    fn test_arm_branch_and_link() {
        load_test("ARM7TDMI/v1/arm_b_bl.json", verify_state, 0);
    }

    #[test]
    fn test_arm_data_proc_immediate() {
        load_test("ARM7TDMI/v1/arm_data_proc_immediate.json", verify_state, 0);
    }

    #[test]
    fn test_arm_data_proc_immediate_shift() {
        load_test("ARM7TDMI/v1/arm_data_proc_immediate_shift.json", verify_state, 0);
    }

    #[test]
    fn test_arm_data_proc_register_shift() {
        load_test("ARM7TDMI/v1/arm_data_proc_register_shift.json", verify_state, 0);
    }

    #[test]
    fn test_arm_mrs() {
        load_test("ARM7TDMI/v1/arm_mrs.json", verify_state, 0);
    }

    #[test]
    fn test_arm_msr_imm() {
        load_test("ARM7TDMI/v1/arm_msr_imm.json", verify_state, 0);
    }

    #[test]
    fn test_arm_msr_reg() {
        load_test("ARM7TDMI/v1/arm_msr_reg.json", verify_state, 0);
    }

    #[test]
    fn test_arm_mul_mla() {
        load_test("ARM7TDMI/v1/arm_mul_mla.json", verify_state_no_carry, 0);
    }

    #[test]
    fn test_arm_mull_mlal() {
        load_test("ARM7TDMI/v1/arm_mull_mlal.json", verify_state_no_carry_overflow, 0);
    }

    #[test]
    fn test_arm_ldr_str_immediate_offset() {
        load_test("ARM7TDMI/v1/arm_ldr_str_immediate_offset.json", verify_state, 0);
    }

    #[test]
    fn test_arm_ldr_str_register_offset() {
        load_test("ARM7TDMI/v1/arm_ldr_str_register_offset.json", verify_state, 0);
    }

    #[test]
    fn test_arm_ldrh_strh() {
        load_test("ARM7TDMI/v1/arm_ldrh_strh.json", verify_state, 0);
    }
}
