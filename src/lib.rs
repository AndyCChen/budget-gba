pub mod arm {
    pub mod constants;
    pub mod core;
    pub mod json_test_states;
    pub mod memory;
    mod opcode_tables {
        pub mod arm_handlers;
        pub mod arm_table_gen;
        pub mod common;
        pub mod instruction_lut;
        pub mod thumb_handlers;
        pub mod thumb_table_gen;
    }
}

pub mod bus {
    pub mod core;
    pub mod test_bus;
}
