mod common;
mod gba_bus;
mod test_bus;

pub trait BusInterface {
    fn reset(&mut self) {}

    fn pipeline_read_word(&mut self, address: u32, access: u8) -> u32;
    fn pipeline_read_halfword(&mut self, address: u32, access: u8) -> u16;

    fn read_word(&mut self, address: u32, access: u8) -> u32;
    fn read_rotate_word(&mut self, address: u32, access: u8) -> u32;
    fn read_halfword(&mut self, address: u32, access: u8) -> u32;
    fn read_rotate_halfword(&mut self, address: u32, access: u8) -> u32;
    fn read_signed_halfword(&mut self, address: u32, access: u8) -> u32;
    fn read_byte(&mut self, address: u32, access: u8) -> u32;
    fn read_signed_byte(&mut self, address: u32, access: u8) -> u32;

    fn write_word(&mut self, address: u32, value: u32, access: u8);
    fn write_halfword(&mut self, address: u32, value: u16, access: u8);
    fn write_byte(&mut self, address: u32, value: u8, access: u8);

    fn i_cycle(&mut self) {}

    fn cycles(&self) -> u64 {
        0
    }
}

pub use gba_bus::{Bus, BusComponents, GbaBus};
pub use test_bus::TestBus;
