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
    } else if (instruction & 0b11_1000_0000) == 0b00_1000_0000 {
        mov_cmp_add_sub_immediate
    } else if (instruction & 0b11_1111_0000) == 0b01_0000_0000 {
        alu_operations
    } else if (instruction & 0b11_1111_0000) == 0b01_0001_0000 {
        add_cmp_mov_hi
    } else if (instruction & 0b11_1110_0000) == 0b01_0010_0000 {
        pc_relative_load
    } else {
        undefined_thumb
    }
}
