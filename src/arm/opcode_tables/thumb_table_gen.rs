use crate::arm::core::Arm7tdmi;

pub(crate) type ThumbHandler = fn(&mut Arm7tdmi, u16);
pub(crate) const THUMB_TABLE_SIZE: usize = 0x400;

pub(crate) const fn generate_thumb_table() -> [ThumbHandler; THUMB_TABLE_SIZE] {
    use crate::arm::opcode_tables::thumb_handlers::*;

    let mut thumb_table: [ThumbHandler; THUMB_TABLE_SIZE] = [undefined_thumb; THUMB_TABLE_SIZE];

    let mut i = 0;

    while i < thumb_table.len() {
        thumb_table[i] = generate_thumb_instruction(i);
        i += 1;
    }

    thumb_table
}

const fn generate_thumb_instruction(_instruction: usize) -> ThumbHandler {
    use crate::arm::opcode_tables::thumb_handlers::*;
    undefined_thumb
}
