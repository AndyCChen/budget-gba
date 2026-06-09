use crate::ppu::Registers;
use crate::scheduler::*;

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
        self.registers = Registers::new();
    }

    pub fn update_vcount(&mut self) {
        let current_count = self.registers.v_counter.scanline_count();
        self.registers
            .v_counter
            .with_scanline_count(current_count + 1);
    }

    pub fn hdraw(&mut self, scheduler: &mut Scheduler) {
        scheduler.add(1007, GbaEvent::HBlank);
    }

    pub fn hblank(&mut self, scheduler: &mut Scheduler) {
        scheduler.add(226, GbaEvent::UpdateVCount);

        if self.registers.v_counter.scanline_count() == 159 {
            scheduler.add(226, GbaEvent::VBlankHDraw);
        } else {
            scheduler.add(226, GbaEvent::HDraw);
        }
    }

    pub fn vblank_hdraw(&mut self, scheduler: &mut Scheduler) {
        scheduler.add(1007, GbaEvent::VBlankHBlank)
    }

    pub fn vblank_hblank(&mut self, scheduler: &mut Scheduler) {
        scheduler.add(226, GbaEvent::UpdateVCount);

        if self.registers.v_counter.scanline_count() == 227 {
            scheduler.add(226, GbaEvent::HDraw);
            self.registers.v_counter.set_scanline_count(0);
        } else {
            scheduler.add(226, GbaEvent::VBlankHDraw);
        }
    }
}
