use super::GbaBus;
use crate::bus::Bus;
use crate::ppu::Ppu;
use num_traits::FromPrimitive;

const BIOS_SIZE: usize = 16 * 1024;
const WRAM_256: usize = 256 * 1024;
const WRAM_32: usize = 32 * 1024;

impl GbaBus {
    pub fn new() -> Self {
        Self {
            bios_ram: vec![0; BIOS_SIZE].into_boxed_slice(),
            wram_256: vec![0; WRAM_256].into_boxed_slice(),
            wram_32: vec![0; WRAM_32].into_boxed_slice(),
            gamepak_rom: vec![0; 0].into_boxed_slice(),
            ppu: Ppu::new(),
        }
    }

    pub fn reset(&mut self) {
        self.bios_ram.fill(0);
        self.wram_256.fill(0);
        self.wram_32.fill(0);
        self.gamepak_rom.fill(0);
    }

    // tick the system for N cycles
    fn tick(&mut self, _n: u8) {}

    fn read<T: GbaBusInt + FromPrimitive>(&mut self, address: u32, _access: u8) -> T {
        let page = address >> 24;
        let address = address & 0x0FFF_FFFF; // upper 4 bits of address is unused

        match page {
            // bios
            0 => {
                self.tick(1);
                T::mem_read(T::align(address & 0x3FFF), &self.bios_ram)
            }

            // 256kb wram
            2 => {
                let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                self.tick(if is_u32 { 6 } else { 3 });
                T::mem_read(T::align(address & 0x3FFFF), &self.wram_256)
            }

            // 32kb wram
            3 => {
                self.tick(1);
                T::mem_read(T::align(address & 0x7FFF), &self.wram_32)
            }

            // I/O registers
            4 => {
                self.tick(1);
                T::io_read(self, T::align(address))
            }

            // palette ram
            5 => {
                let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                self.tick(if is_u32 { 2 } else { 1 });
                T::mem_read(T::align(address & 0x3FF), &self.ppu.palette_ram)
            }

            // vram
            6 => {
                let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                self.tick(if is_u32 { 2 } else { 1 });

                // 96kb vram is mirrored in 128kb blocks
                // 96kb vram can be pictured as 64kb + 32kb, with the 32kb block being mirrored
                let address = address & 0x1_FFFF;
                if address < 0x1_8000 {
                    T::mem_read(T::align(address), &self.ppu.vram)
                } else {
                    let address = 0x1_0000 | (address & 0x7FFF);
                    T::mem_read(T::align(address), &self.ppu.vram)
                }
            }

            // oam ram
            7 => {
                self.tick(1);
                T::mem_read(T::align(address & 0x3FF), &self.ppu.oam)
            }

            _ => todo!("open bus value"),
        }
    }

    fn write<T: GbaBusInt>(&mut self, address: u32, value: T, _access: u8) {
        let page = address >> 24;
        let address = address & 0x0FFF_FFFF; // upper 4 bits of address is unused

        match page {
            // 256 kb wram
            2 => {
                let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                self.tick(if is_u32 { 6 } else { 3 });
                value.mem_write(T::align(address & 0x3FFFF), &mut self.wram_256)
            }

            // 32kb wram
            3 => {
                self.tick(1);
                value.mem_write(T::align(address & 0x7FFF), &mut self.wram_32)
            }

            // I/O registers
            4 => {
                self.tick(1);
                value.io_write(self, T::align(address));
            }

            // palette ram
            5 => match T::int_type() {
                GbaBusIntType::Word | GbaBusIntType::Halfword => {
                    let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                    self.tick(if is_u32 { 2 } else { 1 });
                    value.mem_write(T::align(address & 0x3FF), &mut self.ppu.palette_ram);
                }
                GbaBusIntType::Byte => {
                    self.tick(1);
                    let address = u16::align(address & 0x3FF);
                    // byte sized writes will duplicate the byte in the upper and lower 16 bit halfword in memory
                    value.mem_write(address, &mut self.ppu.palette_ram);
                    value.mem_write(address + 1, &mut self.ppu.palette_ram);
                }
            },

            // vram
            6 => match T::int_type() {
                GbaBusIntType::Word | GbaBusIntType::Halfword => {
                    let is_u32 = matches!(T::int_type(), GbaBusIntType::Word);
                    self.tick(if is_u32 { 2 } else { 1 });

                    let address = address & 0x1_FFFF;
                    if address < 0x1_8000 {
                        value.mem_write(T::align(address), &mut self.ppu.vram);
                    } else {
                        let address = 0x1_0000 | (address & 0x7FFF);
                        value.mem_write(T::align(address), &mut self.ppu.vram);
                    }
                }
                GbaBusIntType::Byte => {
                    self.tick(1);

                    // 96kb vram is mirrored in 128kb blocks
                    // 96kb vram can be pictured as 64kb + 32kb, with the 32kb block being mirrored
                    let address = address & 0x1_FFFF;
                    if address < 0x1_8000 {
                        let address = u16::align(address);
                        value.mem_write(address, &mut self.ppu.vram);
                        value.mem_write(address + 1, &mut self.ppu.vram);
                    } else {
                        let address = T::align(0x1_0000 | (address & 0x7FFF));
                        value.mem_write(address, &mut self.ppu.vram);
                        value.mem_write(address + 1, &mut self.ppu.vram);
                    }
                }
            },

            // oam ram
            7 => match T::int_type() {
                GbaBusIntType::Word | GbaBusIntType::Halfword => {
                    self.tick(1);
                    value.mem_write(T::align(address & 0x3FF), &mut self.ppu.oam);
                }
                GbaBusIntType::Byte => {
                    self.tick(1);
                    let address = u16::align(address & 0x3FF);
                    // byte sized writes will duplicate the byte in the upper and lower 16 bit halfword in memory
                    value.mem_write(address, &mut self.ppu.oam);
                    value.mem_write(address + 1, &mut self.ppu.oam);
                }
            },

            _ => todo!("set open bus value"),
        }
    }
}

impl Bus for GbaBus {
    fn i_cycle(&mut self) {
        self.tick(1);
    }

    fn pipeline_read_word(&mut self, address: u32, access: u8) -> u32 {
        self.read(address, access)
    }

    fn pipeline_read_halfword(&mut self, address: u32, access: u8) -> u16 {
        self.read(address, access)
    }

    fn read_word(&mut self, address: u32, access: u8) -> u32 {
        self.read(address, access)
    }

    fn read_halfword(&mut self, address: u32, access: u8) -> u16 {
        self.read(address, access)
    }

    fn read_byte(&mut self, address: u32, access: u8) -> u8 {
        self.read(address, access)
    }

    fn write_word(&mut self, address: u32, value: u32, access: u8) {
        self.write(address, value, access);
    }

    fn write_halfword(&mut self, address: u32, value: u16, access: u8) {
        self.write(address, value, access);
    }

    fn write_byte(&mut self, address: u32, value: u8, access: u8) {
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
    fn io_read<T: GbaBusInt + FromPrimitive>(bus: &GbaBus, address: usize) -> T;
    fn io_write(&self, bus: &mut GbaBus, address: usize);
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

    fn io_read<T: GbaBusInt + FromPrimitive>(bus: &GbaBus, address: usize) -> T {
        T::from_u8(bus.read_io_byte(address)).unwrap()
    }

    fn io_write(&self, bus: &mut GbaBus, address: usize) {
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

    fn io_read<T: GbaBusInt + FromPrimitive>(bus: &GbaBus, address: usize) -> T {
        T::from_u16(bus.read_io_halfword(address)).unwrap()
    }

    fn io_write(&self, bus: &mut GbaBus, address: usize) {
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

    fn io_read<T: GbaBusInt + FromPrimitive>(bus: &GbaBus, address: usize) -> T {
        T::from_u32(bus.read_io_word(address)).unwrap()
    }

    fn io_write(&self, bus: &mut GbaBus, address: usize) {
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
        arm::access_code,
        bus::{Bus, GbaBus},
    };

    #[test]
    fn bus_read_test() {
        let mut bus = GbaBus::new();
        bus.wram_256[0x3FF00] = 0xAA;
        bus.wram_256[0x3FF01] = 0xBB;
        bus.wram_256[0x3FF02] = 0xCC;
        bus.wram_256[0x3FF03] = 0xDD;

        let wram_256_start = 0x0200_0000;

        // read at aligned addresses

        assert_eq!(
            bus.read_word(wram_256_start + 0x3FF00, access_code::NONSEQUENTIAL),
            0xDDCC_BBAA
        );

        assert_eq!(
            bus.read_halfword(wram_256_start + 0x3FF00, access_code::NONSEQUENTIAL),
            0xBBAA
        );

        assert_eq!(
            bus.read_byte(wram_256_start + 0x3FF00, access_code::NONSEQUENTIAL),
            0xAA
        );

        // read at unaligned addresses

        assert_eq!(
            bus.read_word(wram_256_start + 2 + 0x3FF00, access_code::NONSEQUENTIAL),
            0xDDCC_BBAA
        );

        assert_eq!(
            bus.read_halfword(wram_256_start + 1 + 0x3FF00, access_code::NONSEQUENTIAL),
            0xBBAA
        );
    }

    #[test]
    fn bus_write_test() {
        let wram_256_start = 0x0200_0000;
        let mut bus = GbaBus::new();

        // test writes to aligned addresses

        bus.write_word(wram_256_start, 0xAABB_CCDD, access_code::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[0..4], [0xDD, 0xCC, 0xBB, 0xAA]);

        bus.reset();
        bus.write_halfword(wram_256_start + 2, 0xAABB, access_code::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[2..4], [0xBB, 0xAA]);

        bus.reset();
        bus.write_byte(wram_256_start + 1, 0xFF, access_code::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[1], 0xFF);

        // test writes to unaligned addresses

        bus.reset();
        bus.write_word(wram_256_start + 1, 0xAABB_CCDD, access_code::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[0..4], [0xDD, 0xCC, 0xBB, 0xAA]);

        bus.reset();
        bus.write_halfword(wram_256_start + 3, 0xAABB, access_code::NONSEQUENTIAL);
        assert_eq!(bus.wram_256[2..4], [0xBB, 0xAA]);
    }
}
