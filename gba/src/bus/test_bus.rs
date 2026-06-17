use crate::arm::*;
use crate::bus::{BusInterface, common};
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
        access: AccessCode,
        kind: KindCode,
    ) -> T {
        let data = self.transactions[self.transaction_index].clone();
        self.next_transaction();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access.bits(), "mismatch access code!");
        assert_eq!(usize::from(data.size), size_of::<T>(), "mismatch size!");
        assert_eq!(data.kind, kind.bits(), "mismatch kind!");

        let mask = T::max_value().to_u32().unwrap();
        T::from_u32(data.data & mask).unwrap()
    }

    fn write<T: Unsigned + ToPrimitive>(&mut self, address: u32, value: T, access: AccessCode) {
        let data = self.transactions[self.transaction_index].clone();
        self.next_transaction();

        assert_eq!(data.addr, address, "mismatched address!");
        assert_eq!(data.access, access.bits(), "mismatch access code!");
        assert_eq!(usize::from(data.size), size_of::<T>(), "mismatch size!");
        assert_eq!(data.kind, KindCode::WRITE.bits(), "mismatch kind code!");
        assert_eq!(data.data, value.to_u32().unwrap(), "mismatch write value!");
    }
}

impl BusInterface for TestBus {
    fn pipeline_read_word(&mut self, address: u32, access: AccessCode) -> u32 {
        let address = address & !3; // align 4 byte boundary
        self.read(address, access, KindCode::INSTRUCTION_READ)
    }

    fn pipeline_read_halfword(&mut self, address: u32, access: AccessCode) -> u16 {
        let address = address & !1;
        self.read(address, access, KindCode::INSTRUCTION_READ)
    }

    fn read_word(&mut self, address: u32, access: AccessCode) -> u32 {
        self.read(address, access, KindCode::GENERAL_READ)
    }

    fn read_rotate_word(&mut self, address: u32, access: AccessCode) -> u32 {
        let word: u32 = self.read(address, access, KindCode::GENERAL_READ);
        common::read_rotate_word(address, word)
    }

    fn read_halfword(&mut self, address: u32, access: AccessCode) -> u32 {
        let halfword: u16 = self.read(address, access, KindCode::GENERAL_READ);
        u32::from(halfword)
    }

    fn read_rotate_halfword(&mut self, address: u32, access: AccessCode) -> u32 {
        let halfword: u16 = self.read(address, access, KindCode::GENERAL_READ);
        common::read_rotate_halfword(address, halfword)
    }

    fn read_signed_halfword(&mut self, address: u32, access: AccessCode) -> u32 {
        let halfword: u16 = self.read(address, access, KindCode::GENERAL_READ);
        common::read_signed_halfword(address, halfword)
    }

    fn read_byte(&mut self, address: u32, access: AccessCode) -> u32 {
        let byte: u8 = self.read(address, access, KindCode::GENERAL_READ);
        u32::from(byte)
    }

    fn read_signed_byte(&mut self, address: u32, access: AccessCode) -> u32 {
        let byte: u8 = self.read(address, access, KindCode::GENERAL_READ);
        common::read_signed_byte(byte)
    }

    fn write_word(&mut self, address: u32, value: u32, access: AccessCode) {
        self.write(address, value, access);
    }

    fn write_halfword(&mut self, address: u32, value: u16, access: AccessCode) {
        self.write(address, value, access)
    }

    fn write_byte(&mut self, address: u32, value: u8, access: AccessCode) {
        self.write(address, value, access)
    }

    /// Test bus has no notion of tracking cpu cyles.
    fn get_timestamp(&self) -> u64 {
        0
    }

    /// Do not care about interrupts for test bus
    fn interrupt_requested(&self) -> bool {
        false
    }
}
