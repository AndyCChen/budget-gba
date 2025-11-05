use crate::arm::core::Arm7tdmi;

impl Arm7tdmi {
    pub fn pipeline_read_word(&mut self, address: u32, access: u8) -> u32 {
        self.bus.pipeline_read_word(address, access)
    }

    pub fn pipeline_read_halfword(&mut self, address: u32, access: u8) -> u32 {
        self.bus.pipeline_read_halfword(address, access).into()
    }

    pub fn read_rotate_word(&mut self, address: u32, access: u8) -> u32 {
        let value = self.bus.read_word(address, access);
        value.rotate_right((address & 3) * 8)
    }

    pub fn read_word(&mut self, address: u32, access: u8) -> u32 {
        self.bus.read_word(address, access)
    }

    pub fn _read_halfword(&mut self, address: u32, access: u8) -> u32 {
        self.bus.read_halfword(address, access).into()
    }

    pub fn read_rotate_halfword(&mut self, address: u32, access: u8) -> u32 {
        let value: u32 = self.bus.read_halfword(address, access).into();
        value.rotate_right((address & 1) * 8)
    }

    pub fn read_signed_halfword(&mut self, address: u32, access: u8) -> u32 {
        if address & 1 == 1 {
            self.read_rotate_halfword(address, access)  as i8 as i32 as u32
            //self.bus.read_byte(address, access) as i8 as i32 as u32
        } else {
            self.bus.read_halfword(address, access) as i16 as i32 as u32
        }
    }

    pub fn read_byte(&mut self, address: u32, access: u8) -> u32 {
        self.bus.read_byte(address, access).into()
    }

    pub fn read_signed_byte(&mut self, address: u32, access: u8) -> u32 {
        self.bus.read_byte(address, access) as i8 as i32 as u32
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
