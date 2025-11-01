use serde::{Deserialize, Serialize};

#[allow(non_snake_case)]
#[derive(Serialize, Deserialize, Debug)]
pub struct State {
    pub R: [u32; 16],
    pub R_fiq: [u32; 7],
    pub R_svc: [u32; 2],
    pub R_abt: [u32; 2],
    pub R_irq: [u32; 2],
    pub R_und: [u32; 2],
    pub CPSR: u32,
    pub SPSR: [u32; 5],
    pub pipeline: [u32; 2],
    pub access: u8,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Transactions {
    pub kind: u8,
    pub size: u8,
    pub addr: u32,
    pub data: u32,
    pub access: u8,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct InputStates {
    pub initial: State,
    pub r#final: State,
    pub transactions: Vec<Transactions>,
    pub opcode: u32,
    pub base_addr: u32,
}
