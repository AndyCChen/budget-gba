use crate::arm::core::Arm7tdmi;

pub fn undefined_thumb(_cpu: &mut Arm7tdmi, opcode: u16) {
    todo!("handle undefined opcode: {opcode}");
}
