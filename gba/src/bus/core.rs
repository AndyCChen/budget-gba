use std::fmt::Display;

use crate::apu::Apu;
use crate::bus::BusInterface;
use crate::bus::common;
use crate::gamepak::{AccessType, GamePak, GamepakRegion};
use crate::keypad::Keypad;
use crate::ppu::Ppu;
use crate::scheduler::{GbaEvent::*, Scheduler};
use crate::arm::AccessCode;
use num_traits::FromPrimitive;

const BIOS_SIZE: usize = 16 * 1024;
const WRAM_256: usize = 256 * 1024;
const WRAM_32: usize = 32 * 1024;

pub struct Bus {
    pub gamepak: GamePak,
    pub ppu: Ppu,
    pub apu: Apu,
    pub keypad: Keypad,
    pub bios_ram: [u8; BIOS_SIZE],
    pub wram_256: [u8; WRAM_256],
    pub wram_32: [u8; WRAM_32],
    pub scheduler: Scheduler,
}

impl Bus {
    pub fn new() -> Self {
        let mut scheduler = Scheduler::new(32);
        let ppu = Ppu::new(&mut scheduler);

        Self {
            scheduler,
            gamepak: GamePak::new(),
            ppu,
            apu: Apu::new(),
            keypad: Keypad::new(),
            bios_ram: [0; BIOS_SIZE],
            wram_256: [0; WRAM_256],
            wram_32: [0; WRAM_32],
        }
    }

    pub fn reset(&mut self) {
        self.scheduler.clear();
        self.gamepak.reset();
        self.ppu.reset();
        self.apu.reset();
        self.keypad.reset();
        self.bios_ram.fill(0);
        self.wram_256.fill(0);
        self.wram_32.fill(0);
    }

    fn read<T: GbaBusInt + FromPrimitive>(&mut self, address: u32, access: AccessCode) -> T {
        let page = address >> 24;
        let address = address & 0x0FFF_FFFF; // upper 4 bits of address is unused

        match page {
            // bios
            0 => {
                self.step(1);
                T::mem_read(T::align(address & 0x3FFF), &self.bios_ram)
            }

            // 256kb wram, always has 2 wait states
            2 => {
                let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                self.step(if is_u32 { 6 } else { 3 });
                T::mem_read(T::align(address & 0x3FFFF), &self.wram_256)
            }

            // 32kb wram
            3 => {
                self.step(1);
                T::mem_read(T::align(address & 0x7FFF), &self.wram_32)
            }

            // I/O registers
            4 => {
                self.step(1);
                T::io_read(self, T::align(address))
            }

            // palette ram
            5 => {
                let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                self.step(if is_u32 { 2 } else { 1 });
                T::mem_read(T::align(address & 0x3FF), &self.ppu.mem.palette_ram)
            }

            // vram
            6 => {
                let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                self.step(if is_u32 { 2 } else { 1 });

                // 96kb vram is mirrored in 128kb blocks
                // 96kb vram can be pictured as 64kb + 32kb, with the 32kb block being mirrored
                let address = address & 0x1_FFFF;
                if address < 0x1_8000 {
                    T::mem_read(T::align(address), &self.ppu.mem.vram)
                } else {
                    let address = 0x1_0000 | (address & 0x7FFF);
                    T::mem_read(T::align(address), &self.ppu.mem.vram)
                }
            }

            // oam ram
            7 => {
                self.step(1);
                T::mem_read(T::align(address & 0x3FF), &self.ppu.mem.oam)
            }

            // gamepak region 8/9
            8 => {
                let address = address & 0xFFFFFF;
                let wait_states = self.gamepak.get_wait_states(
                    AccessType::try_from(access).unwrap(),
                    GamepakRegion::Region8_9,
                );
                self.step(1 + wait_states); // timing is 1 plus number of waitstates
                let rom_size = self.gamepak.rom.len() as u32;
                T::mem_read(T::align(address % rom_size), &self.gamepak.rom)
            }

            // gamepak region 10/11
            10 => {
                let address = address & 0xFFFFFF;
                let wait_states = self.gamepak.get_wait_states(
                    AccessType::try_from(access).unwrap(),
                    GamepakRegion::Region10_11,
                );
                self.step(1 + wait_states); // timing is 1 plus number of waitstates
                let rom_size = self.gamepak.rom.len() as u32;
                T::mem_read(T::align(address % rom_size), &self.gamepak.rom)
            }

            // gamepak region 12/13
            12 => {
                let address = address & 0xFFFFFF;
                let wait_states = self.gamepak.get_wait_states(
                    AccessType::try_from(access).unwrap(),
                    GamepakRegion::Region12_13,
                );
                self.step(1 + wait_states); // timing is 1 plus number of waitstates
                let rom_size = self.gamepak.rom.len() as u32;
                T::mem_read(T::align(address % rom_size), &self.gamepak.rom)
            }

            _ => todo!("read open bus value"),
        }
    }

    fn write<T: GbaBusInt + Display>(&mut self, address: u32, value: T, _access: AccessCode) {
        let page = address >> 24;
        let address = address & 0x0FFF_FFFF; // upper 4 bits of address is unused

        match page {
            // 256 kb wram
            2 => {
                let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                self.step(if is_u32 { 6 } else { 3 });
                value.mem_write(T::align(address & 0x3FFFF), &mut self.wram_256)
            }

            // 32kb wram
            3 => {
                self.step(1);
                value.mem_write(T::align(address & 0x7FFF), &mut self.wram_32)
            }

            // I/O registers
            4 => {
                self.step(1);
                value.io_write(self, T::align(address));
            }

            // palette ram
            5 => match T::int_type() {
                GbaBusIntType::Word | GbaBusIntType::Halfword => {
                    let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                    self.step(if is_u32 { 2 } else { 1 });
                    value.mem_write(T::align(address & 0x3FF), &mut self.ppu.mem.palette_ram);
                }
                GbaBusIntType::Byte => {
                    self.step(1);
                    let address = u16::align(address & 0x3FF);
                    // byte sized writes will duplicate the byte in the upper and lower 16 bit halfword in memory
                    value.mem_write(address, &mut self.ppu.mem.palette_ram);
                    value.mem_write(address + 1, &mut self.ppu.mem.palette_ram);
                }
            },

            // vram
            6 => match T::int_type() {
                GbaBusIntType::Word | GbaBusIntType::Halfword => {
                    let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                    self.step(if is_u32 { 2 } else { 1 });

                    let address = address & 0x1_FFFF;
                    if address < 0x1_8000 {
                        value.mem_write(T::align(address), &mut self.ppu.mem.vram);
                    } else {
                        let address = 0x1_0000 | (address & 0x7FFF);
                        value.mem_write(T::align(address), &mut self.ppu.mem.vram);
                    }
                }
                GbaBusIntType::Byte => {
                    self.step(1);
                    // 96kb vram is mirrored in 128kb blocks
                    // 96kb vram can be pictured as 64kb + 32kb, with the 32kb block being mirrored
                    let address = address & 0x1_FFFF;
                    if address < 0x1_8000 {
                        let address = u16::align(address);
                        value.mem_write(address, &mut self.ppu.mem.vram);
                        value.mem_write(address + 1, &mut self.ppu.mem.vram);
                    } else {
                        let address = T::align(0x1_0000 | (address & 0x7FFF));
                        value.mem_write(address, &mut self.ppu.mem.vram);
                        value.mem_write(address + 1, &mut self.ppu.mem.vram);
                    }
                }
            },

            // oam ram
            7 => match T::int_type() {
                GbaBusIntType::Word | GbaBusIntType::Halfword => {
                    self.step(1);
                    value.mem_write(T::align(address & 0x3FF), &mut self.ppu.mem.oam);
                }
                GbaBusIntType::Byte => {
                    self.step(1);
                    let address = u16::align(address & 0x3FF);
                    // byte sized writes will duplicate the byte in the upper and lower 16 bit halfword in memory
                    value.mem_write(address, &mut self.ppu.mem.oam);
                    value.mem_write(address + 1, &mut self.ppu.mem.oam);
                }
            },

            _ => todo!("write set open bus value, address: {address:08X}, value: {value}"),
        }
    }

    fn step(&mut self, cycles: u8) {
        self.scheduler.step(cycles);

        while let Some(gba_event) = self.scheduler.poll_event() {
            match gba_event {
                HDraw => self.ppu.hdraw(&mut self.scheduler),
                HBlank => self.ppu.hblank(&mut self.scheduler),
                VBlankHDraw => self.ppu.vblank_hdraw(&mut self.scheduler),
                VBlankHBlank => self.ppu.vblank_hblank(&mut self.scheduler),
                UpdateVCount => self.ppu.update_vcount(),
                ToggleVBlankFlag(flag) => self.ppu.toggle_vblank_flag(flag),
            }
        }
    }
}

impl BusInterface for Bus {
    fn i_cycle(&mut self) {
        self.scheduler.step(1);
    }

    fn get_timestamp(&self) -> u64 {
        self.scheduler.get_timestamp()
    }

    fn pipeline_read_word(&mut self, address: u32, access: AccessCode) -> u32 {
        self.read(address, access)
    }

    fn pipeline_read_halfword(&mut self, address: u32, access: AccessCode) -> u16 {
        self.read(address, access)
    }

    fn read_word(&mut self, address: u32, access: AccessCode) -> u32 {
        self.read(address, access)
    }

    fn read_rotate_word(&mut self, address: u32, access: AccessCode) -> u32 {
        let word: u32 = self.read(address, access);
        common::read_rotate_word(address, word)
    }

    fn read_halfword(&mut self, address: u32, access: AccessCode) -> u32 {
        let halfword: u16 = self.read(address, access);
        u32::from(halfword)
    }

    fn read_rotate_halfword(&mut self, address: u32, access: AccessCode) -> u32 {
        let halfword: u16 = self.read(address, access);
        common::read_rotate_halfword(address, halfword)
    }

    fn read_signed_halfword(&mut self, address: u32, access: AccessCode) -> u32 {
        let halfword: u16 = self.read(address, access);
        common::read_signed_halfword(address, halfword)
    }

    fn read_byte(&mut self, address: u32, access: AccessCode) -> u32 {
        let byte: u8 = self.read(address, access);
        u32::from(byte)
    }

    fn read_signed_byte(&mut self, address: u32, access: AccessCode) -> u32 {
        let byte: u8 = self.read(address, access);
        common::read_signed_byte(byte)
    }

    fn write_word(&mut self, address: u32, value: u32, access: AccessCode) {
        self.write(address, value, access);
    }

    fn write_halfword(&mut self, address: u32, value: u16, access: AccessCode) {
        self.write(address, value, access);
    }

    fn write_byte(&mut self, address: u32, value: u8, access: AccessCode) {
        self.write(address, value, access);
    }
}

enum GbaBusIntType {
    Word,
    Halfword,
    Byte,
}

trait GbaBusInt {
    fn mem_read<T: FromPrimitive>(address: usize, data: &[u8]) -> T;
    fn mem_write(&self, address: usize, data: &mut [u8]);
    fn io_read<T: GbaBusInt + FromPrimitive>(bus: &Bus, address: usize) -> T;
    fn io_write(&self, bus: &mut Bus, address: usize);
    fn align(address: u32) -> usize;
    fn int_type() -> GbaBusIntType;
}

impl GbaBusInt for u8 {
    fn mem_read<T: FromPrimitive>(address: usize, data: &[u8]) -> T {
        T::from_u8(data[address]).unwrap()
    }

    fn mem_write(&self, address: usize, data: &mut [u8]) {
        data[address] = *self;
    }

    fn io_read<T: GbaBusInt + FromPrimitive>(bus: &Bus, address: usize) -> T {
        T::from_u8(bus.read_io_byte(address)).unwrap()
    }

    fn io_write(&self, bus: &mut Bus, address: usize) {
        bus.write_io_byte(*self, address);
    }

    fn align(address: u32) -> usize {
        address as usize
    }

    fn int_type() -> GbaBusIntType {
        GbaBusIntType::Byte
    }
}

impl GbaBusInt for u16 {
    fn mem_read<T: FromPrimitive>(address: usize, data: &[u8]) -> T {
        T::from_u16(u16::from_le_bytes(
            data[address..address + 2].try_into().unwrap(),
        ))
        .unwrap()
    }

    fn mem_write(&self, address: usize, data: &mut [u8]) {
        data[address..address + 2].copy_from_slice(&self.to_le_bytes());
    }

    fn io_read<T: GbaBusInt + FromPrimitive>(bus: &Bus, address: usize) -> T {
        T::from_u16(bus.read_io_halfword(address)).unwrap()
    }

    fn io_write(&self, bus: &mut Bus, address: usize) {
        bus.write_io_halfword(*self, address);
    }

    fn align(address: u32) -> usize {
        (address & !1) as usize
    }

    fn int_type() -> GbaBusIntType {
        GbaBusIntType::Halfword
    }
}

impl GbaBusInt for u32 {
    fn mem_read<T: FromPrimitive>(address: usize, data: &[u8]) -> T {
        T::from_u32(u32::from_le_bytes(
            data[address..address + 4].try_into().unwrap(),
        ))
        .unwrap()
    }

    fn mem_write(&self, address: usize, data: &mut [u8]) {
        data[address..address + 4].copy_from_slice(&self.to_le_bytes());
    }

    fn io_read<T: GbaBusInt + FromPrimitive>(bus: &Bus, address: usize) -> T {
        T::from_u32(bus.read_io_word(address)).unwrap()
    }

    fn io_write(&self, bus: &mut Bus, address: usize) {
        bus.write_io_word(*self, address);
    }

    fn align(address: u32) -> usize {
        (address & !3) as usize
    }

    fn int_type() -> GbaBusIntType {
        GbaBusIntType::Word
    }
}

#[cfg(test)]
mod gba_bus_test {
    use crate::{
        arm::AccessCode,
        bus::{Bus, BusInterface},
    };

    #[test]
    fn bus_read_test() {
        let mut bus = Bus::new();

        bus.wram_256[0x3FF00] = 0xAA;
        bus.wram_256[0x3FF01] = 0xBB;
        bus.wram_256[0x3FF02] = 0xCC;
        bus.wram_256[0x3FF03] = 0xDD;

        let wram_256_start = 0x0200_0000;

        // read at aligned addresses

        assert_eq!(
            bus.read_word(wram_256_start + 0x3FF00, AccessCode::NONSEQUENTIAL),
            0xDDCC_BBAA
        );

        assert_eq!(
            bus.read_halfword(wram_256_start + 0x3FF00, AccessCode::NONSEQUENTIAL),
            0xBBAA
        );

        assert_eq!(
            bus.read_byte(wram_256_start + 0x3FF00, AccessCode::NONSEQUENTIAL),
            0xAA
        );

        // read at unaligned addresses

        assert_eq!(
            bus.read_word(wram_256_start + 2 + 0x3FF00, AccessCode::NONSEQUENTIAL),
            0xDDCC_BBAA
        );

        assert_eq!(
            bus.read_halfword(wram_256_start + 1 + 0x3FF00, AccessCode::NONSEQUENTIAL),
            0xBBAA
        );
    }

    #[test]
    fn bus_write_test() {
        let mut bus = Bus::new();

        let wram_256_start = 0x0200_0000;

        // test writes to aligned addresses

        bus.write_word(wram_256_start, 0xAABB_CCDD, AccessCode::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[0..4], [0xDD, 0xCC, 0xBB, 0xAA]);

        bus.reset();
        bus.write_halfword(wram_256_start + 2, 0xAABB, AccessCode::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[2..4], [0xBB, 0xAA]);

        bus.reset();
        bus.write_byte(wram_256_start + 1, 0xFF, AccessCode::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[1], 0xFF);

        // test writes to unaligned addresses

        bus.reset();
        bus.write_word(wram_256_start + 1, 0xAABB_CCDD, AccessCode::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[0..4], [0xDD, 0xCC, 0xBB, 0xAA]);

        bus.reset();
        bus.write_halfword(wram_256_start + 3, 0xAABB, AccessCode::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[2..4], [0xBB, 0xAA]);
    }
}
