use crate::{arm::*, bus::core::Bus};

use num_traits::{Bounded, FromPrimitive, ToPrimitive, Unsigned};

pub struct TestBus {
    transaction_index: usize,
    transactions: Vec<Transactions>,
}

impl TestBus {
    pub fn new(transactions: &[Transactions]) -> Self {
        Self {
            transaction_index: 0,
            transactions: transactions.to_vec(),
        }
    }

    fn next_transaction(&mut self) {
        self.transaction_index += 1;
    }

    fn read<T: Unsigned + Bounded + FromPrimitive + ToPrimitive>(
        &mut self,
        address: u32,
        access: u8,
        kind: u8,
    ) -> T {
        let data = self.transactions[self.transaction_index].clone();
        self.next_transaction();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(usize::from(data.size), size_of::<T>(), "mismatch size!");
        assert_eq!(data.kind, kind, "mismatch kind!");

        let mask = T::max_value().to_u32().unwrap();
        T::from_u32(data.data & mask).unwrap()
    }

    fn write<T: Unsigned + ToPrimitive>(
        &mut self,
        address: u32,
        value: T,
        access: u8,
    ) {
        let data = self.transactions[self.transaction_index].clone();
        self.next_transaction();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access, "mismatch access code!");
        assert_eq!(usize::from(data.size), size_of::<T>(), "mismatch size!");
        assert_eq!(data.kind, kind_code::WRITE, "mismatch kind code!");
        assert_eq!(data.data, value.to_u32().unwrap(), "mismatch write value!");
    }
}

impl Bus for TestBus {
    fn pipeline_read_word(&mut self, address: u32, access: u8) -> u32 {
        let address = address & !3; // align 4 byte boundary
        self.read(address, access, kind_code::INSTRUCTION_READ)
    }

    fn pipeline_read_halfword(&mut self, address: u32, access: u8) -> u16 {
        let address = address & !1;
        self.read(address, access, kind_code::INSTRUCTION_READ)
    }

    fn read_word(&mut self, address: u32, access: u8) -> u32 {
        self.read(address, access, kind_code::GENERAL_READ)
    }

    fn read_halfword(&mut self, address: u32, access: u8) -> u16 {
        self.read(address, access, kind_code::GENERAL_READ)
    }

    fn read_byte(&mut self, address: u32, access: u8) -> u8 {
        self.read(address, access, kind_code::GENERAL_READ)
    }

    fn write_word(&mut self, address: u32, value: u32, access: u8) {
        self.write(address, value, access);
    }

    fn write_halfword(&mut self, address: u32, value: u16, access: u8) {
        self.write(address, value, access)
    }

    fn write_byte(&mut self, address: u32, value: u8, access: u8) {
        self.write(address, value, access)
    }
}
