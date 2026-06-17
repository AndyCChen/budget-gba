use crate::arm::arm_json_test_states::*;
use crate::arm::constants::AccessCode;
use crate::arm::decoder_tables::*;
use crate::arm::opcode_tables::{
    ARM_TABLE_SIZE, ArmHandler, CONDITION_TABLE, THUMB_TABLE_SIZE, ThumbHandler,
    generate_arm_table, generate_thumb_table,
};
use crate::bus::BusInterface;
use bitfield_struct::*;
use std::num::Wrapping;

#[derive(Default)]
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

#[bitfield(u32)]
pub struct StatusRegister {
    /// unsure if default mode starts in User or System mode.
    /// mgba seems to start in System mode and I think user and system mode are the same on the gba.
    #[bits(5, default = Mode::System, from = Mode::from_bits)]
    pub mode_bits: Mode,

    #[bits(1, default = CpuMode::ArmMode)]
    pub t: CpuMode,

    /// 0: enable fiq, 1: disable fiq
    pub f: bool,

    /// 0: enable irq, 1: disable irq
    pub i: bool,

    #[bits(20)]
    /// reserved
    __: u32,

    /// overflow
    pub v: bool,

    /// carry flag
    pub c: bool,

    /// zero flag
    pub z: bool,

    /// negative flag
    pub n: bool,
}

#[derive(Clone, Debug)]
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

#[bitenum]
#[derive(Clone, Debug)]
#[repr(u8)]
pub enum CpuMode {
    #[fallback]
    ArmMode = 0,
    ThumbMode = 1,
}

pub struct Arm7tdmi<T: BusInterface> {
    pub registers: GeneralRegisters,
    pub status: StatusRegisters,
    pub pipeline: [CpuInstruction; 2], // make sure pipeline is filled first before running!
    pub pipeline_state: AccessCode,
    pub instruction_log_enable: bool,
    arm_table: [ArmHandler<T>; ARM_TABLE_SIZE],
    thumb_table: [ThumbHandler<T>; THUMB_TABLE_SIZE],
    instruction_buffer: RingBuffer<(u32, CpuInstruction)>,
}

#[derive(Debug, Clone, Copy)]
pub enum CpuInstruction {
    Arm(u32),
    Thumb(u32),
}

impl Default for CpuInstruction {
    fn default() -> Self {
        Arm(0)
    }
}

use CpuInstruction::*;
use CpuMode::*;

impl<T: BusInterface> Arm7tdmi<T> {
    pub fn new() -> Self {
        Self {
            registers: GeneralRegisters::default(),
            status: StatusRegisters::default(),
            pipeline: [Arm(0); 2],
            pipeline_state: AccessCode::NONSEQUENTIAL,
            instruction_log_enable: false,
            instruction_buffer: RingBuffer::new(32),
            arm_table: generate_arm_table(),
            thumb_table: generate_thumb_table(),
        }
    }

    pub fn test_init() -> Self {
        Self {
            registers: GeneralRegisters::default(),
            status: StatusRegisters::default(),
            pipeline: [Arm(0); 2],
            pipeline_state: AccessCode::NONSEQUENTIAL,
            instruction_buffer: RingBuffer::new(50_000),
            instruction_log_enable: false,
            arm_table: generate_arm_table(),
            thumb_table: generate_thumb_table(),
        }
    }

    pub fn update_test_state(&mut self, input_state: &InputStates) {
        let pipeline = match StatusRegister::from_bits(input_state.initial.CPSR).t() {
            ArmMode => [
                Arm(input_state.initial.pipeline[0]),
                Arm(input_state.initial.pipeline[1]),
            ],
            ThumbMode => [
                Thumb(input_state.initial.pipeline[0]),
                Thumb(input_state.initial.pipeline[1]),
            ],
        };

        self.pipeline = pipeline;
        self.pipeline_state = AccessCode::from_bits(input_state.initial.access).unwrap();

        self.registers = GeneralRegisters {
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
        };

        self.status = StatusRegisters {
            cpsr: StatusRegister::from_bits(input_state.initial.CPSR),
            spsr_fiq: StatusRegister::from_bits(input_state.initial.SPSR[0]),
            spsr_svc: StatusRegister::from_bits(input_state.initial.SPSR[1]),
            spsr_abt: StatusRegister::from_bits(input_state.initial.SPSR[2]),
            spsr_irq: StatusRegister::from_bits(input_state.initial.SPSR[3]),
            spsr_und: StatusRegister::from_bits(input_state.initial.SPSR[4]),
        };
    }

    pub fn reset(&mut self) {
        self.registers = GeneralRegisters::default();
        self.status = StatusRegisters::default();
        self.pipeline.fill(Arm(0));
        self.pipeline_state = AccessCode::NONSEQUENTIAL;
        // self.instruction_buffer.clear();
    }

    pub fn step(&mut self, bus: &mut T) {
        // handle interrupt is one is requested and interupts are enable in cpsr
        if !self.status.cpsr.i() && bus.interrupt_requested() {
            self.do_interrupt(bus);
        }

        let instruction = self.pipeline[0];
        let pc = self.registers.r15.0;

        if self.instruction_log_enable {
            self.instruction_buffer.push_back((pc, instruction));
        }

        match instruction {
            Arm(arm_instr) => {
                self.pipeline_prefetch(bus, ArmMode);
                let condition = (arm_instr & 0xF000_0000) >> 24;
                let flags = self.status.cpsr.into_bits() >> 28;

                if CONDITION_TABLE[(condition | flags) as usize] {
                    let arm_table_hash =
                        ((arm_instr & 0x0FF00000) >> 16) | ((arm_instr & 0xF0) >> 4);
                    self.arm_table[arm_table_hash as usize](self, bus, arm_instr);
                } else {
                    self.pipeline_state = AccessCode::SEQUENTIAL | AccessCode::CODE;
                    self.registers.r15 += 4;
                }
            }
            Thumb(thumb_instr) => {
                self.pipeline_prefetch(bus, ThumbMode);
                let thumb_table_hash = (thumb_instr >> 6) & 0x3FF;
                self.thumb_table[thumb_table_hash as usize](self, bus, thumb_instr as u16);
            }
        };
    }

    pub fn print_log(&mut self) {
        if let Some((pc, instruction)) = self.instruction_buffer.iter().next() {
            let asm_string = match instruction {
                Arm(opcode) => decode_arm(*opcode).to_asm_string(*pc),
                Thumb(opcode) => decode_thumb(*opcode as u16).to_asm_string(*pc),
            };

            let pc = match instruction {
                Arm(_) => pc.wrapping_sub(8),
                Thumb(_) => pc.wrapping_sub(4),
            };

            println!("{pc:08X}    {asm_string}");
        }
    }

    /// Retrieve register in arm mode
    pub fn get_banked_register(&self, register_id: u32) -> u32 {
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
                "Register id must be in range 0-15! {register_id} {:?}",
                self.status.cpsr.mode_bits()
            ),
        }
    }

    pub fn set_banked_register(&mut self, register_id: u32, value: u32) {
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
                "Register id must be in range 0-15! {register_id} {:?}",
                self.status.cpsr.mode_bits()
            ),
        }
    }

    /// Retrieve banked spsr from current corresponding mode.
    /// If mode is user/system, returns the cpsr
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

    /// Set banked spsr of the current corresponding mode.
    /// No spsr exists for User and System mode.
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

    /// Flush and refills the pipeline for arm mode
    pub fn pipeline_refill_arm(&mut self, bus: &mut T) {
        self.pipeline[0] = Arm(bus.pipeline_read_word(
            self.registers.r15.0,
            AccessCode::CODE | AccessCode::NONSEQUENTIAL,
        ));
        self.pipeline[1] = Arm(bus.pipeline_read_word(
            self.registers.r15.0.wrapping_add(4),
            AccessCode::CODE | AccessCode::SEQUENTIAL,
        ));

        self.pipeline_state = AccessCode::SEQUENTIAL | AccessCode::CODE;
        self.registers.r15 += 8;
    }

    /// Flush and refills the pipeline for thumb mode
    pub fn pipeline_refill_thumb(&mut self, bus: &mut T) {
        self.pipeline[0] = Thumb(
            bus.pipeline_read_halfword(
                self.registers.r15.0,
                AccessCode::CODE | AccessCode::NONSEQUENTIAL,
            )
            .into(),
        );
        self.pipeline[1] = Thumb(
            bus.pipeline_read_halfword(
                self.registers.r15.0.wrapping_add(2),
                AccessCode::CODE | AccessCode::SEQUENTIAL,
            )
            .into(),
        );

        self.pipeline_state = AccessCode::SEQUENTIAL | AccessCode::CODE;
        self.registers.r15 += 4;
    }

    /// fetch opcode and push into pipeline
    fn pipeline_prefetch(&mut self, bus: &mut T, mode: CpuMode) {
        self.pipeline.copy_within(1.., 0);
        match mode {
            ArmMode => {
                self.registers.r15 &= !0x3;
                self.pipeline[1] =
                    Arm(bus.pipeline_read_word(self.registers.r15.0, self.pipeline_state));
            }
            ThumbMode => {
                self.registers.r15 &= !0x1;
                self.pipeline[1] = Thumb(
                    bus.pipeline_read_halfword(self.registers.r15.0, self.pipeline_state)
                        .into(),
                );
            }
        }
    }

    fn do_interrupt(&mut self, bus: &mut T) {
        self.status.spsr_irq = self.status.cpsr;
        self.status.cpsr.set_mode_bits(Mode::Irq);

        match self.status.cpsr.t() {
            ArmMode => self.registers.r14_irq = self.registers.r15.0.wrapping_sub(4),
            ThumbMode => self.registers.r14_irq = self.registers.r15.0,
        }

        // jump to bios interrupt vector at 0x18.
        self.registers.r15 = Wrapping(0x18);
        self.status.cpsr.set_i(true); // Disable further interrupt from occuring
        self.status.cpsr.set_t(CpuMode::ArmMode);
        self.pipeline_refill_arm(bus);
    }
}

#[rustfmt::skip]
#[allow(dead_code)]
mod test_utils {
    use crate::bus::TestBus;
    use crate::arm::decoder_tables::*;

    use super::*;
    use std::fs;
    use std::io::Write;
    use std::path::PathBuf;

    pub fn load_test(
        test_file: &str,
        check_state: fn(cpu: &Arm7tdmi<TestBus>, input_state: &InputStates, test_num: usize),
        skip: usize,
    ) {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let test_file_path = PathBuf::from(manifest_dir).join("ARM7TDMI/v1").join(test_file);
        let file_name = test_file_path.file_stem().unwrap().to_str().unwrap().to_string();
        
        let Ok(data) = fs::read_to_string(test_file_path) else {
            panic!("Failed to load test file!");
        };

        let items: Vec<InputStates> = serde_json::from_str(&data).unwrap();
        let it = items.iter().enumerate().skip(skip);

        let mut cpu = Arm7tdmi::test_init();
        cpu.instruction_log_enable =  true;
        for (count, item) in it {
            cpu.update_test_state(item);
            cpu.step(&mut TestBus::new(&item.transactions));
            check_state(&cpu, item, count);
            cpu.reset();
        }
        
        let output_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/decoder_output");
        fs::create_dir_all(output_dir).expect("failed to create directory!");
        let mut file = fs::File::create(format!("{output_dir}/{file_name}.txt")).expect("failed to create file");

        for (pc, instruction) in cpu.instruction_buffer.iter()  {
            let asm_string = match instruction {
                Arm(opcode32) => decode_arm(*opcode32).to_asm_string(*pc),
                Thumb(opcode16) => decode_thumb(*opcode16 as u16).to_asm_string(*pc),
            };

            writeln!(file, "{asm_string}").expect("write failed!");           
        }
    }

    // ignore checking carry flag, useful for checking muliply instruction as the carry flag result is not emulated
    pub fn verify_state_no_carry(cpu: &Arm7tdmi<TestBus>, input_state: &InputStates, test_num: usize) {
        let mask = 0xDFFF_FFFF;
        assert_eq!(cpu.status.cpsr.into_bits() & mask, input_state.r#final.CPSR & mask, "{input_state:#?} cspr, test: {test_num}");
        verify_state_core(cpu, input_state, test_num);
    }

    pub fn verify_state_no_carry_overflow(cpu: &Arm7tdmi<TestBus>, input_state: &InputStates, test_num: usize) {
        let mask = 0xCFFF_FFFF;
        assert_eq!(cpu.status.cpsr.into_bits() & mask, input_state.r#final.CPSR & mask, "{input_state:#?} cspr, test: {test_num}");
        verify_state_core(cpu, input_state, test_num);
    }

    pub fn verify_state(cpu: &Arm7tdmi<TestBus>, input_state: &InputStates, test_num: usize) {
        assert_eq!(cpu.status.cpsr.into_bits(), input_state.r#final.CPSR, "{input_state:#?} cspr, test: {test_num}");
        verify_state_core(cpu, input_state, test_num);
    }
    
    fn verify_state_core(cpu: &Arm7tdmi<TestBus>, input_state: &InputStates, test_num: usize) {
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

        let pipeline = cpu.pipeline.map(|instruction| match instruction {
            Arm(instr) => instr,
            Thumb(instr) => instr,
        });

        assert_eq!(pipeline[0], final_state.pipeline[0], "{input_state:#?} pipeline_0, test: {test_num}");
        assert_eq!(pipeline[1], final_state.pipeline[1], "{input_state:#?} pipeline_1, test: {test_num}");
    }
}

#[cfg(test)]
#[rustfmt::skip]
mod arm_32_tests {
    use super::test_utils::*;

    #[test]
    fn test_arm_branch_and_exchange() {
        load_test("arm_bx.json", verify_state, 0);
    }

    #[test]
    fn test_arm_branch_and_link() {
        load_test("arm_b_bl.json", verify_state, 0);
    }

    #[test]
    fn test_arm_data_proc_immediate() {
        load_test("arm_data_proc_immediate.json", verify_state, 0);
    }

    #[test]
    fn test_arm_data_proc_immediate_shift() {
        load_test("arm_data_proc_immediate_shift.json", verify_state, 0);
    }

    #[test]
    fn test_arm_data_proc_register_shift() {
        load_test("arm_data_proc_register_shift.json", verify_state, 0);
    }

    #[test]
    fn test_arm_mrs() {
        load_test("arm_mrs.json", verify_state, 0);
    }

    #[test]
    fn test_arm_msr_imm() {
        load_test("arm_msr_imm.json", verify_state, 0);
    }

    #[test]
    fn test_arm_msr_reg() {
        load_test("arm_msr_reg.json", verify_state, 0);
    }

    #[test]
    fn test_arm_mul_mla() {
        load_test("arm_mul_mla.json", verify_state_no_carry, 0);
    }

    #[test]
    fn test_arm_mull_mlal() {
        load_test("arm_mull_mlal.json", verify_state_no_carry_overflow, 0);
    }

    #[test]
    fn test_arm_ldr_str_immediate_offset() {
        load_test("arm_ldr_str_immediate_offset.json", verify_state, 0);
    }

    #[test]
    fn test_arm_ldr_str_register_offset() {
        load_test("arm_ldr_str_register_offset.json", verify_state, 0);
    }

    #[test]
    fn test_arm_ldrh_strh() {
        load_test("arm_ldrh_strh.json", verify_state, 0);
    }

    #[test]
    fn test_arm_ldrsb_ldrsh() {
        load_test("arm_ldrsb_ldrsh.json", verify_state, 0);
    }

    #[test]
    fn test_arm_ldm_stm() {
        load_test("arm_ldm_stm.json", verify_state, 0);
    }

    #[test]
    fn test_arm_swp() {
        load_test("arm_swp.json", verify_state, 0);
    }

    #[test]
    fn test_arm_swi() {
        load_test("arm_swi.json", verify_state, 0);
    }
}

#[cfg(test)]
#[rustfmt::skip]
mod thumb_16_tests {
    use super::test_utils::{load_test, verify_state, verify_state_no_carry};
   
    #[test]
    fn test_thumb_lsl_lsr_asr() {
        load_test("thumb_lsl_lsr_asr.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_add_sub() {
        load_test("thumb_add_sub.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_mov_cmp_add_sub() {
        load_test("thumb_mov_cmp_add_sub.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_data_proc() {
        use super::*;
        use crate::bus::TestBus;
        use std::fs;
        use std::io::Write;
        
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let Ok(data) = fs::read_to_string(format!("{manifest_dir}/ARM7TDMI/v1/thumb_data_proc.json")) else {
            panic!("Failed to load test file!");
        };

        let items: Vec<InputStates> = serde_json::from_str(&data).unwrap();
        let is_multiply = |opcode: u32| (opcode >> 6) & 0xF == 0b1101;

        let mut cpu = Arm7tdmi::test_init();
        cpu.instruction_log_enable = true;

        items
            .iter()
            .filter(|item| !is_multiply(item.opcode))
            .enumerate()
            .for_each(|(count, item)| {
                cpu.update_test_state(item);
                cpu.step(&mut TestBus::new(&item.transactions));
                verify_state(&cpu, item, count);
                cpu.reset();
            });

        items
            .iter()
            .filter(|item| is_multiply(item.opcode))
            .enumerate()
            .for_each(|(count, item)| {
                cpu.update_test_state(item);
                cpu.step(&mut TestBus::new(&item.transactions));
                verify_state_no_carry(&cpu, item, count);
                cpu.reset();
            });

        let output_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/decoder_output");
        fs::create_dir_all(output_dir).expect("failed to create directory!");
        let mut file = fs::File::create(format!("{output_dir}/thumb_data_proc.txt")).expect("failed to create file");

        for (pc, instruction) in cpu.instruction_buffer.iter() {
            let asm_string = match instruction {
                Arm(opcode32) => decode_arm(*opcode32).to_asm_string(*pc),
                Thumb(opcode16) => decode_thumb(*opcode16 as u16).to_asm_string(*pc),
            };

            writeln!(file, "{asm_string}").expect("write failed!");           
        }
    }

    #[test]
    fn test_thumb_bx() {
        load_test("thumb_bx.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_add_cmp_mov_hi() {
        load_test("thumb_add_cmp_mov_hi.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldr_pc_rel() {
        load_test("thumb_ldr_pc_rel.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldr_str_reg_offset() {
        load_test("thumb_ldr_str_reg_offset.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldrsb_strb_reg_offset() {
       load_test("thumb_ldrsb_strb_reg_offset.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldrsh_ldrsb_reg_offset() {
        load_test("thumb_ldrsh_ldrsb_reg_offset.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldrh_strh_reg_offset() {
        load_test("thumb_ldrh_strh_reg_offset.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldr_str_imm_offset() {
        load_test("thumb_ldr_str_imm_offset.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldrb_strb_imm_offset() {
        load_test("thumb_ldrb_strb_imm_offset.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldrh_strh_imm_offset() {
        load_test("thumb_ldrh_strh_imm_offset.json", verify_state, 0);
    }
    
    #[test]
    fn test_thumb_ldr_str_sp_rel() {
        load_test("thumb_ldr_str_sp_rel.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_add_sp_or_pc() {
        load_test("thumb_add_sp_or_pc.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_add_sub_sp() {
        load_test("thumb_add_sub_sp.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_push_pop() {
        load_test("thumb_push_pop.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_ldm_stm() {
        load_test("thumb_ldm_stm.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_bcc() {
        load_test("thumb_bcc.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_swi() {
        load_test("thumb_swi.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_b() {
        load_test("thumb_b.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_bl_blx_prefix() {
        load_test("thumb_bl_blx_prefix.json", verify_state, 0);
    }

    #[test]
    fn test_thumb_bl_suffix() {
        load_test("thumb_bl_suffix.json", verify_state, 0);
    }
}
