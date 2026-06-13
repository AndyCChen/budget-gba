use bitfield_struct::bitfield;

#[bitfield(u16)]
pub struct Rgb5 {
    #[bits(5)]
    pub red: u8,

    #[bits(5)]
    pub green: u8,

    #[bits(5)]
    pub blue: u8,

    __: bool,
}

impl Rgb5 {
    pub const fn white() -> Self {
        Self::new().with_red(31).with_green(31).with_blue(31)
    }

    pub const fn black() -> Self {
        Self::new().with_red(0).with_green(0).with_blue(0)
    }

    pub fn to_rgba8_array(&self) -> [u8; 4] {
        let red = f32::from(self.red()) / 31.0 * 255.0;
        let blue = f32::from(self.blue()) / 31.0 * 255.0;
        let green = f32::from(self.green()) / 31.0 * 255.0;

        [
            red.round() as u8,
            green.round() as u8,
            blue.round() as u8,
            255,
        ]
    }

    pub fn from_u16(value: u16) -> Self {
        Self::from_bits(0x7FFF & value)
    }
}

pub const DISPLAY_WIDTH: usize = 240;
pub const DISPLAY_HEIGHT: usize = 160;
pub type DisplayBuffer = [[Rgb5; DISPLAY_WIDTH]; DISPLAY_HEIGHT];
