use register_macros::gba_register;

pub struct Registers {
    pub sound_bias: SoundBias,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            sound_bias: SoundBias::new(),
        }
    }
}

#[gba_register(u16)]
pub struct SoundBias {
    #[bits(1)]
    __: u8,

    #[bits(9, default = 0x100)]
    pub bias_level: u16,

    #[bits(4)]
    __: u8,

    #[bits(2)]
    pub sampling_cycle: u8,
}
