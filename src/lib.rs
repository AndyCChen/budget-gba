pub mod arm {
    pub mod arm_handlers;
    pub mod constants;
    pub mod core;
    pub mod memory;
    pub mod instruction_lut;
    pub mod json_test_states;
}

pub mod bus {
    pub mod core;
    pub mod test_bus;
}
