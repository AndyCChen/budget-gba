pub trait Bus {
    fn pipeline_read_word(&mut self, address: u32, access: u8) -> u32;
    fn pipeline_read_halfword(&mut self, address: u32, access: u8) -> u16;

    fn read_word(&mut self, address: u32, access: u8) -> u32;
    fn read_halfword(&mut self, address: u32, access: u8) -> u16;
    fn read_byte(&mut self, address: u32, access: u8) -> u8;

    fn write_word(&mut self, address: u32, value: u32, access: u8);
    fn write_halfword(&mut self, address: u32, value: u16, access: u8);
    fn write_byte(&mut self, address: u32, value: u8, access: u8);
}
