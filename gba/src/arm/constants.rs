use bitflags::bitflags;

// 16.78 MHz clock rate
pub const ARM7TDMI_CLOCK_RATE: usize = 16 * 1024 * 1024;

bitflags! {
    #[derive(Clone, Copy)]
    pub struct AccessCode: u8 {
        const NONSEQUENTIAL = 0;
        const SEQUENTIAL = 1 << 0;
        const CODE = 1 << 1;
        const DMA = 1 << 2;
        const LOCK = 1 << 3;
    }
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct KindCode: u8 {
        const INSTRUCTION_READ = 0;
        const GENERAL_READ = 1 << 0;
        const WRITE = 1 << 1;
    }
}
