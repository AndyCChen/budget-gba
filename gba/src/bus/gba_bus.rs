use crate::bus::Bus;
use num_traits::FromPrimitive;

const BIOS_SIZE: usize = 16 * 1024;
const WRAM_256: usize = 256 * 1024;
const WRAM_32: usize = 32 * 1024;

pub struct GbaBus {
    bios_ram: [u8; BIOS_SIZE],
    wram_256: [u8; WRAM_256],
    wram_32: [u8; WRAM_32],
    gamepak_rom: Vec<u8>,
}

impl GbaBus {
    pub fn new() -> Self {
        Self {
            bios_ram: [0; BIOS_SIZE],
            wram_256: [0; WRAM_256],
            wram_32: [0; WRAM_32],
            gamepak_rom: vec![0; 0],
        }
    }

    pub fn reset(&mut self) {
        self.bios_ram.fill(0);
        self.wram_256.fill(0);
        self.wram_32.fill(0);
        self.gamepak_rom.fill(0);
    }

    fn read<T: GbaBusInt + FromPrimitive>(&mut self, address: u32, _access: u8) -> T {
        let page = address >> 24;
        let address = address & 0x0FFF_FFFF; // upper 4 bits of address is unused

        match page {
            // bios
            0 => T::mem_read(T::align(address & 0x3FFF), &self.bios_ram),

            // 256kb wram
            2 => T::mem_read(T::align(address & 0x3FFFF), &self.wram_256),

            // 32kb wram
            3 => T::mem_read(T::align(address & 0x7FFF), &self.wram_32),
            _ => todo!(),
        }
    }

    fn write<T: GbaBusInt>(&mut self, address: u32, value: T, _access: u8) {
        let page = address >> 24;
        let address = address & 0x0FFF_FFFF; // upper 4 bits of address is unused

        match page {
            // 256 kb wram
            2 => value.mem_write(T::align(address & 0x3FFFF), &mut self.wram_256),

            // 32kb wram
            3 => value.mem_write(T::align(address & 0x7FFF), &mut self.wram_32),

            _ => (),
        }
    }
}

impl Bus for GbaBus {
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

trait GbaBusInt {
    fn mem_read<T: FromPrimitive>(address: usize, data: &[u8]) -> T;
    fn mem_write(&self, address: usize, data: &mut [u8]);
    fn align(address: u32) -> usize;
}

impl GbaBusInt for u8 {
    fn mem_read<T: FromPrimitive>(address: usize, data: &[u8]) -> T {
        T::from_u8(data[address]).unwrap()
    }

    fn mem_write(&self, address: usize, data: &mut [u8]) {
        data[address] = *self;
    }

    fn align(address: u32) -> usize {
        address as usize
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

    fn align(address: u32) -> usize {
        (address & !1) as usize
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

    fn align(address: u32) -> usize {
        (address & !3) as usize
    }
}

#[cfg(test)]
mod gba_bus_test {
    use crate::{
        arm::access_code,
        bus::{Bus, gba_bus::GbaBus},
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
