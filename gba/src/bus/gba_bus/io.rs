use crate::bus::GbaBus;
use crate::io::constants::*;
use crate::io::*;

impl GbaBus {
    pub fn read_io_byte(&self, address: usize) -> u8 {
        match address {
            // lcd I/O registers

            DISPCNT_0 => self.ppu.registers.lcd_control.read(HalfwordIo::B0),
            DISPCNT_1 => self.ppu.registers.lcd_control.read(HalfwordIo::B1),

            DISPSTAT_0 => self.ppu.registers.lcd_status.read(HalfwordIo::B0),
            DISPSTAT_1 => self.ppu.registers.lcd_status.read(HalfwordIo::B1),

            VCOUNT_0 => self.ppu.registers.v_counter.read(HalfwordIo::B0),
            VCOUNT_1 => self.ppu.registers.v_counter.read(HalfwordIo::B1),

            BG0CNT_0 => self.ppu.registers.bg_control_0.read(HalfwordIo::B0),
            BG0CNT_1 => self.ppu.registers.bg_control_0.read(HalfwordIo::B1),

            // Interrupt, Waitstate, and Power-Down Control

            WAITCNT_0 => self.gamepak.registers.waitstate_control.read(WordIo::B0),
            WAITCNT_1 => self.gamepak.registers.waitstate_control.read(WordIo::B1),
            WAITCNT_2 => self.gamepak.registers.waitstate_control.read(WordIo::B2),
            WAITCNT_3 => self.gamepak.registers.waitstate_control.read(WordIo::B3),

            _ => 0,
        }
    }

    pub fn write_io_byte(&mut self, value: u8, address: usize) {
        match address {
            // lcd I/O registers
            DISPCNT_0 => self.ppu.registers.lcd_control.write(value, HalfwordIo::B0),
            DISPCNT_1 => self.ppu.registers.lcd_control.write(value, HalfwordIo::B1),

            DISPSTAT_0 => self.ppu.registers.lcd_status.write(value, HalfwordIo::B0),
            DISPSTAT_1 => self.ppu.registers.lcd_status.write(value, HalfwordIo::B1),

            BG0CNT_0 => self.ppu.registers.bg_control_0.write(value, HalfwordIo::B0),
            BG0CNT_1 => self.ppu.registers.bg_control_0.write(value, HalfwordIo::B1),

            // Interrupt, Waitstate, and Power-Down Control

            WAITCNT_0 => self.gamepak.registers.waitstate_control.write(value, WordIo::B0),
            WAITCNT_1 => self.gamepak.registers.waitstate_control.write(value, WordIo::B1),
            WAITCNT_2 => self.gamepak.registers.waitstate_control.write(value, WordIo::B2),
            WAITCNT_3 => self.gamepak.registers.waitstate_control.write(value, WordIo::B3),

            _ => (),
        }
    }

    pub fn read_io_halfword(&self, address: usize) -> u16 {
        let halfword = [self.read_io_byte(address), self.read_io_byte(address + 1)];
        u16::from_le_bytes(halfword)
    }

    pub fn write_io_halfword(&mut self, value: u16, address: usize) {
        value
            .to_le_bytes()
            .iter()
            .enumerate()
            .for_each(|(offset, byte)| {
                self.write_io_byte(*byte, address + offset);
            });
    }

    pub fn read_io_word(&self, address: usize) -> u32 {
        let word = [
            self.read_io_byte(address),
            self.read_io_byte(address + 1),
            self.read_io_byte(address + 2),
            self.read_io_byte(address + 3),
        ];
        u32::from_le_bytes(word)
    }

    pub fn write_io_word(&mut self, value: u32, address: usize) {
        value
            .to_le_bytes()
            .iter()
            .enumerate()
            .for_each(|(offset, byte)| {
                self.write_io_byte(*byte, address + offset);
            });
    }
}
