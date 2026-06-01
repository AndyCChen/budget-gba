pub fn read_rotate_word(address: u32, word: u32) -> u32 {
    word.rotate_right((address & 3) * 8)
}

pub fn read_rotate_halfword(address: u32, halfword: u16) -> u32 {
    let word = u32::from(halfword);
    word.rotate_right((address & 1) * 8)
}


pub fn read_signed_halfword(address: u32, halfword: u16) -> u32 {
    if address & 1 == 1 {
        read_rotate_halfword(address, halfword) as i8 as i32 as u32
    }
    else {
        halfword as i16 as i32 as u32
    }
}

pub fn read_signed_byte(byte: u8) -> u32 {
    byte as i8 as i32 as u32
}
