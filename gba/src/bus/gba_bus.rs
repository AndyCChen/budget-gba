use crate::bus::Bus;
use num_traits::{Bounded, FromPrimitive, ToPrimitive, Unsigned};

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

    fn read<T: Unsigned + FromPrimitive>(&mut self, address: u32, _access: u8) -> T {
        let address = address & 0x0FFF_FFFF; // upper 4 bits of address is unused
        let read_value: T;

        // bios
        if address <= 0x01FF_FFFF {
            read_value = mem_read(align::<T>(address & 0x3FFF), &self.bios_ram);
        } else {
            todo!()
        }

        read_value
    }

    fn write<T: Unsigned + FromPrimitive>(&mut self, address: u32, value: T, access: u8) {
        let address = address & 0x0FFF_FFFF; // upper 4 bits of address is unused

        
    }
}

fn mem_read<T: Unsigned + FromPrimitive>(address: usize, data: &[u8]) -> T {
    match size_of::<T>() {
        1 => T::from_u8(data[address]),
        2 => T::from_u16(u16::from_le_bytes(
            data[address..address + 2].try_into().unwrap(),
        )),
        4 => T::from_u32(u32::from_le_bytes(
            data[address..address + 4].try_into().unwrap(),
        )),
        _ => panic!(),
    }
    .unwrap()
}

fn align<T: Unsigned>(address: u32) -> usize {
    (address & !(size_of::<T>() as u32 - 1)) as usize
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

#[cfg(test)]
mod gba_bus_alignment_test {
    use super::align;

    #[test]
    fn test_word_align() {
        // test aligning a already aligned address
        assert_eq!(align::<u32>(0), 0);
        assert_eq!(align::<u32>(4), 4);
        assert_eq!(align::<u32>(8), 8);

        // test aligning a address outside of 4 byte boundaries
        assert_eq!(align::<u32>(1), 0);
        assert_eq!(align::<u32>(2), 0);
        assert_eq!(align::<u32>(3), 0);
        assert_eq!(align::<u32>(5), 4);
        assert_eq!(align::<u32>(6), 4);
        assert_eq!(align::<u32>(7), 4);
    }

    #[test]
    fn test_halfword_align() {
        // test aligning a already aligned address
        assert_eq!(align::<u16>(0), 0);
        assert_eq!(align::<u16>(2), 2);
        assert_eq!(align::<u16>(4), 4);

        // test aligning a address outside of 2 byte boundaries
        assert_eq!(align::<u16>(1), 0);
        assert_eq!(align::<u16>(3), 2);
        assert_eq!(align::<u16>(5), 4);
    }

    #[test]
    fn test_byte_align() {
        // byte alignment should leave address unchanged
        assert_eq!(align::<u8>(0), 0);
        assert_eq!(align::<u8>(2), 2);
        assert_eq!(align::<u8>(4), 4);
        assert_eq!(align::<u8>(1), 1);
        assert_eq!(align::<u8>(3), 3);
        assert_eq!(align::<u8>(5), 5);
    }

    #[test]
    fn test_boundary_alignment() {
        // test that the alignment means that address never goes above u32::MAX
    }
}
