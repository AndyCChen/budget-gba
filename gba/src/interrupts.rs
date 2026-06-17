use register_macros::gba_register;

pub struct Interrupt {
    pub interrupt_enable: InterruptEnable,
    pub interrupt_flags: InterruptFlags,
    pub master_interrupt: MasterInterruptEnable,
}

impl Interrupt {
    pub fn new() -> Self {
        Self {
            interrupt_enable: InterruptEnable::default(),
            interrupt_flags: InterruptFlags::default(),
            master_interrupt: MasterInterruptEnable::default(),
        }
    }
}

#[gba_register(u16)]
pub struct MasterInterruptEnable {
    /// 0: Disable interrupts, 1: Enable interrupts
    pub enable: bool,

    #[bits(15)]
    __: u16,
}

#[gba_register(u16)]
pub struct InterruptEnable {
    pub vblank: bool,
    pub hblank: bool,
    pub vcounter_match: bool,
    pub timer0_overflow: bool,
    pub timer1_overflow: bool,
    pub timer2_overflow: bool,
    pub timer3_overflow: bool,
    pub serial_communication: bool,
    pub dma0: bool,
    pub dma1: bool,
    pub dma2: bool,
    pub dma3: bool,
    pub keypad: bool,
    pub gamepak: bool,

    #[bits(2)]
    __: u8,
}

/// Interrupt request/acknowledge flags
#[gba_register(u16)]
pub struct InterruptFlags {
    pub vblank: bool,
    pub hblank: bool,
    pub vcounter_match: bool,
    pub timer0_overflow: bool,
    pub timer1_overflow: bool,
    pub timer2_overflow: bool,
    pub timer3_overflow: bool,
    pub serial_communication: bool,
    pub dma0: bool,
    pub dma1: bool,
    pub dma2: bool,
    pub dma3: bool,
    pub keypad: bool,
    pub gamepak: bool,

    #[bits(2)]
    __: u8,
}
