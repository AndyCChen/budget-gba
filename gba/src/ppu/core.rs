use crate::ppu::Registers;

const PALETTE_SIZE: usize = 1024;
const VRAM_SIZE: usize = 1024 * 96;
const OAM_SIZE: usize = 1024;

pub struct Ppu {
    pub palette_ram: [u8; PALETTE_SIZE],
    pub vram: [u8; VRAM_SIZE],
    pub oam: [u8; OAM_SIZE],

    pub registers: Registers,
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            palette_ram: [0; PALETTE_SIZE],
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE],

            registers: Registers::new(),
        }
    }

    pub fn reset(&mut self) {
        self.palette_ram.fill(0);
        self.vram.fill(0);
        self.oam.fill(0);
    }
}
