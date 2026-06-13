mod common;
mod core;
mod io;
mod test_bus;

pub use core::Bus;
pub use test_bus::TestBus;

use crate::arm::AccessCode;

pub trait BusInterface {
    fn pipeline_read_word(&mut self, address: u32, access: AccessCode) -> u32;
    fn pipeline_read_halfword(&mut self, address: u32, access: AccessCode) -> u16;

    fn read_word(&mut self, address: u32, access: AccessCode) -> u32;
    fn read_rotate_word(&mut self, address: u32, access: AccessCode) -> u32;

    #[allow(dead_code)]
    fn read_halfword(&mut self, address: u32, access: AccessCode) -> u32;

    fn read_rotate_halfword(&mut self, address: u32, access: AccessCode) -> u32;
    fn read_signed_halfword(&mut self, address: u32, access: AccessCode) -> u32;
    fn read_byte(&mut self, address: u32, access: AccessCode) -> u32;
    fn read_signed_byte(&mut self, address: u32, access: AccessCode) -> u32;

    fn write_word(&mut self, address: u32, value: u32, access: AccessCode);
    fn write_halfword(&mut self, address: u32, value: u16, access: AccessCode);
    fn write_byte(&mut self, address: u32, value: u8, access: AccessCode);

    fn i_cycle(&mut self) {}

    #[allow(dead_code)]
    fn get_timestamp(&self) -> u64 {
        0
    }
}
