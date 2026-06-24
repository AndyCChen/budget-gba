use crate::common::*;
use crate::interrupts::InterruptFlags;
use crate::ppu::Registers;
use crate::ppu::backgrounds::*;
use crate::ppu::registers::BgMode;
use crate::scheduler::*;

const PALETTE_SIZE: usize = 1024;
const VRAM_SIZE: usize = 1024 * 96;
const OAM_SIZE: usize = 1024;

pub type PaletteRam = [u8; PALETTE_SIZE];

pub struct Ppu {
    pub mem: Box<Memory>,
    pub registers: Registers,
    pub display_buffer: Box<DisplayBuffer>,
    is_frame_complete: bool,
}

pub struct Memory {
    pub palette_ram: PaletteRam,
    pub vram: [u8; VRAM_SIZE],
    pub oam: [u8; OAM_SIZE],
}

impl Memory {
    fn new() -> Self {
        Self {
            palette_ram: [0; PALETTE_SIZE],
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE],
        }
    }
}

impl Ppu {
    pub fn new(scheduler: &mut Scheduler) -> Self {
        scheduler.add(0, GbaEvent::HDraw);
        Self {
            mem: Box::new(Memory::new()),
            registers: Registers::new(),
            display_buffer: Box::new([[Rgb5::black(); DISPLAY_WIDTH]; DISPLAY_HEIGHT]),
            is_frame_complete: false,
        }
    }

    pub fn reset(&mut self) {
        self.mem.palette_ram.fill(0);
        self.mem.vram.fill(0);
        self.mem.oam.fill(0);
        self.registers = Registers::new();
        self.is_frame_complete = false;
        self.display_buffer
            .iter_mut()
            .flatten()
            .for_each(|rbg_5| *rbg_5 = Rgb5::black());
    }

    pub fn update_vcount(&mut self, interrupt_request: &mut InterruptFlags) {
        let new_count = self.registers.v_counter.scanline_count() + 1;
        self.registers.v_counter.set_scanline_count(new_count);

        if new_count == self.registers.lcd_status.vcount() {
            self.registers.lcd_status.set_v_counter_flag(true);

            if self.registers.lcd_status.vblank_irq_enable() {
                interrupt_request.set_vcounter_match(true);
            }
        } else {
            self.registers.lcd_status.set_v_counter_flag(false);
        }
    }

    pub fn hdraw(&mut self, scheduler: &mut Scheduler) {
        self.registers.lcd_status.set_hblank_flag(false);
        scheduler.add(1007, GbaEvent::HBlank);

        match self.registers.lcd_control.bg_mode() {
            BgMode::Mode0 => draw_mode0(self),
            BgMode::Mode1 => (),
            BgMode::Mode2 => (),
            BgMode::Mode3 => draw_mode3(self),
            BgMode::Mode4 => draw_mode4(self),
            BgMode::Mode5 => draw_mode5(self),
        }
    }

    pub fn hblank(&mut self, scheduler: &mut Scheduler, interrupt_request: &mut InterruptFlags) {
        if self.registers.lcd_status.hblank_irq_enable() {
            interrupt_request.set_hblank(true);
        }

        scheduler.add(225, GbaEvent::UpdateVCount);

        if self.registers.v_counter.scanline_count() == 159 {
            scheduler.add(225, GbaEvent::VBlankHDraw);
            scheduler.add(225, GbaEvent::ToggleVBlankFlag(true));
        } else {
            scheduler.add(225, GbaEvent::HDraw);
        }

        self.registers.lcd_status.set_hblank_flag(true);
    }

    pub fn vblank_hdraw(&mut self, scheduler: &mut Scheduler) {
        scheduler.add(1007, GbaEvent::VBlankHBlank);
        self.registers.lcd_status.set_hblank_flag(false);
    }

    pub fn vblank_hblank(
        &mut self,
        scheduler: &mut Scheduler,
        interrupt_request: &mut InterruptFlags,
    ) {
        if self.registers.lcd_status.hblank_irq_enable() {
            interrupt_request.set_hblank(true);
        }

        if self.registers.v_counter.scanline_count() == 227 {
            self.registers.v_counter.set_scanline_count(0);
            scheduler.add(225, GbaEvent::HDraw);
        } else {
            scheduler.add(225, GbaEvent::UpdateVCount);
            scheduler.add(225, GbaEvent::VBlankHDraw);
        }

        if self.registers.v_counter.scanline_count() == 226 {
            scheduler.add(225, GbaEvent::ToggleVBlankFlag(false));
        }

        self.registers.lcd_status.set_hblank_flag(true);
    }

    pub fn toggle_vblank_flag(&mut self, flag: bool, interrupt_request: &mut InterruptFlags) {
        if flag && self.registers.lcd_status.vblank_irq_enable() {
            interrupt_request.set_vblank(true);
        }

        self.registers.lcd_status.set_vblank_flag(flag);
        self.is_frame_complete = flag;
    }

    /// Read the status of frame complete and resets it.
    pub fn is_frame_complete(&mut self) -> bool {
        let flag = self.is_frame_complete;
        self.is_frame_complete = false;
        flag
    }

    /// Read the status of frame complete without reseting it.
    pub fn is_frame_complete_retain(&self) -> bool {
        self.is_frame_complete
    }
}
