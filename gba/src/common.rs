use bitfield_struct::bitfield;

#[bitfield(u16)]
pub struct Rgb555 {
    #[bits(5)]
    red: u8,

    #[bits(5)]
    green: u8,

    #[bits(5)]
    blue: u8,

    __: bool,
}

impl Rgb555 {
    pub const fn white() -> Self {
        Self::new().with_red(0).with_green(0).with_blue(0)
    }
}

pub const DISPLAY_WIDTH: usize = 240;
pub const DISPLAY_HEIGHT: usize = 160;
pub type DisplayBuffer = [[Rgb555; DISPLAY_WIDTH]; DISPLAY_HEIGHT];
