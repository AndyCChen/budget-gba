use crate::arm::core::Arm7tdmi;

impl Arm7tdmi {
    pub fn pipeline_read_word(&mut self, address: u32, access: u8) -> u32 {
        let address = address & !3; // align 4 byte boundary
        self.bus.pipeline_read_word(address, access)
    }

    pub fn pipeline_read_halfword(&mut self, address: u32, access: u8) -> u16 {
        let address = address & !1;
        self.bus.pipeline_read_halfword(address, access)
    }

    pub fn read_rotate_word(&mut self, address: u32, access: u8) -> u32 {
        let value = self.bus.read_word(address, access);
        value.rotate_right((address & 3) * 8)
    }

    pub fn read_word(&mut self, address: u32, access: u8) -> u32 {
        self.bus.read_word(address, access)
    }

    pub fn read_halfword(&mut self, address: u32, access: u8) -> u16 {
        self.bus.read_halfword(address, access)
    }

    pub fn read_byte(&mut self, address: u32, access: u8) -> u8 {
        self.bus.read_byte(address, access)
    }

    pub fn write_word(&mut self, address: u32, value: u32, access: u8) {
        self.bus.write_word(address, value, access);
    }

    pub fn write_halfword(&mut self, address: u32, value: u16, access: u8) {
        self.bus.write_halfword(address, value, access);
    }

    pub fn write_byte(&mut self, address: u32, value: u8, access: u8) {
        self.bus.write_byte(address, value, access);
    }
}
