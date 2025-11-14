use crate::ppu::Ppu;

pub struct GbaBus {
    bios_ram: Box<[u8]>,
    wram_256: Box<[u8]>,
    wram_32: Box<[u8]>,
    gamepak_rom: Box<[u8]>,
    ppu: Ppu,
}

mod bus;
mod io;
mod io_constants;
