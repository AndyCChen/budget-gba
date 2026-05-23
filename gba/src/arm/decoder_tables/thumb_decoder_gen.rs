use crate::arm::decoder_tables::ThumbInstruction;
use crate::arm::decoder_tables::thumb_decoder::*;
use crate::arm::opcode_tables::THUMB_TABLE_SIZE;

pub type ThumbDecoder = fn(u16) -> ThumbInstruction;

pub const fn generate_thumb_decoder_table() -> [ThumbDecoder; THUMB_TABLE_SIZE] {
    let mut thumb_table: [ThumbDecoder; THUMB_TABLE_SIZE] = [undefined_thumb; THUMB_TABLE_SIZE];

    let mut i = 0;
    while i < THUMB_TABLE_SIZE {
        thumb_table[i] = generate_thumb_decoder(i);
        i += 1;
    }

    thumb_table
}

const fn generate_thumb_decoder(instruction: usize) -> ThumbDecoder {
    if (instruction & 0b11_1110_0000) == 0b00_0110_0000 {
        add_subtract
    } else if (instruction & 0b11_1000_0000) == 0b00_0000_0000 {
        move_shifted
    } else {
        undefined_thumb
    }
}
