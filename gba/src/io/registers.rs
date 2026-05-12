pub enum HalfwordIo {
    B0, // 1st byte
    B1, // 2nd byte
}

pub trait ReadIoHalfWord {
    fn read(&self, byte_select: HalfwordIo) -> u8;
}

pub trait WriteIoHalfword {
    fn write(&mut self, value: u8, byte_select: HalfwordIo);
}

pub enum WordIo {
    B0, // 1st byte
    B1, // etc...
    B2,
    B3,
}

pub trait ReadIoWord {
    fn read(&self, byte_select: WordIo) -> u8;
}

pub trait WriteIoWord {
    fn write(&mut self, value: u8, byte_select: WordIo);
}
