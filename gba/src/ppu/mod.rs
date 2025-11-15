mod registers;

pub use registers::{HalfwordIo, ReadIoHalfWord, WriteIoHalfword};

use registers::Registers;

pub struct Ppu {
    pub palette_ram: Box<[u8]>,
    pub vram: Box<[u8]>,
    pub oam: Box<[u8]>,

    pub registers: Registers,
}

const PALETTE_SIZE: usize = 1024;
const VRAM_SIZE: usize = 1024 * 96;
const OAM_SIZE: usize = 1024;

impl Ppu {
    pub fn new() -> Self {
        Self {
            palette_ram: vec![0; PALETTE_SIZE].into_boxed_slice(),
            vram: vec![0; VRAM_SIZE].into_boxed_slice(),
            oam: vec![0; OAM_SIZE].into_boxed_slice(),

            registers: Registers::new(),
        }
    }
}
