pub mod constants;

pub enum HalfwordIo {
    B0, // 1st byte
    B1, // 2nd byte
}

pub trait ReadIo16 {
    fn read(&self, byte_select: HalfwordIo) -> u8;
}

pub trait WriteIo16 {
    fn write(&mut self, value: u8, byte_select: HalfwordIo);
}

pub enum WordIo {
    B0, // 1st byte
    B1, // etc...
    B2,
    B3,
}

pub trait ReadIo32 {
    fn read(&self, byte_select: WordIo) -> u8;
}

pub trait WriteIo32 {
    fn write(&mut self, value: u8, byte_select: WordIo);
}
