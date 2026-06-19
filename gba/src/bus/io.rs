use crate::bus::Bus;
use crate::io::constants::*;
use crate::io::*;

impl Bus {
    #[rustfmt::skip]
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

            // APU

            SOUNDBIAS_0 => self.apu.registers.sound_bias.read(HalfwordIo::B0),
            SOUNDBIAS_1 => self.apu.registers.sound_bias.read(HalfwordIo::B1),

            // keypad
             
            KEYINPUT_0 => self.keypad.keypad_state.read(HalfwordIo::B0),
            KEYINPUT_1 => self.keypad.keypad_state.read(HalfwordIo::B1),
            KEYCNT_0 => self.keypad.interrupt_control.read(HalfwordIo::B0),
            KEYCNT_1 => self.keypad.interrupt_control.read(HalfwordIo::B1),

            // Interrupt, Waitstate, and Power-Down Control
            IE_0 => self.interrupt.interrupt_enable.read(HalfwordIo::B0),
            IE_1 => self.interrupt.interrupt_enable.read(HalfwordIo::B1),
            IF_0 => self.interrupt.interrupt_flags.read(HalfwordIo::B0),
            IF_1 => self.interrupt.interrupt_flags.read(HalfwordIo::B1),
            WAITCNT_0 => self.gamepak.registers.waitstate_control.read(HalfwordIo::B0),
            WAITCNT_1 => self.gamepak.registers.waitstate_control.read(HalfwordIo::B1),
            IME_0 => self.interrupt.master_interrupt.read(HalfwordIo::B0),
            IME_1 => self.interrupt.master_interrupt.read(HalfwordIo::B1),

            _ => 0,
        }
    }

    #[rustfmt::skip]
    pub fn write_io_byte(&mut self, value: u8, address: usize) {
        match address {
            // lcd I/O registers
             
            DISPCNT_0 => self.ppu.registers.lcd_control.write(value, HalfwordIo::B0),
            DISPCNT_1 => self.ppu.registers.lcd_control.write(value, HalfwordIo::B1),

            DISPSTAT_0 => self.ppu.registers.lcd_status.write(value, HalfwordIo::B0),
            DISPSTAT_1 => self.ppu.registers.lcd_status.write(value, HalfwordIo::B1),

            BG0CNT_0 => self.ppu.registers.bg_control_0.write(value, HalfwordIo::B0),
            BG0CNT_1 => self.ppu.registers.bg_control_0.write(value, HalfwordIo::B1),

            // APU

            SOUNDBIAS_0 => self.apu.registers.sound_bias.write(value, HalfwordIo::B0),
            SOUNDBIAS_1 => self.apu.registers.sound_bias.write(value, HalfwordIo::B1),

            // keypad
             
            KEYCNT_0 => self.keypad.interrupt_control.write(value, HalfwordIo::B0),
            KEYCNT_1 => self.keypad.interrupt_control.write(value, HalfwordIo::B1),

            // Interrupt, Waitstate, and Power-Down Control
            IE_0 => self.interrupt.interrupt_enable.write(value, HalfwordIo::B0),
            IE_1 => self.interrupt.interrupt_enable.write(value, HalfwordIo::B1),

            
            // writing a 1 clears/acknowledges any interrupt flags that are set
            IF_0 => {
                let flags = self.interrupt.interrupt_flags.read(HalfwordIo::B0);
                let value = (value & flags) ^ flags;
                self.interrupt.interrupt_flags.write(value, HalfwordIo::B0)
            },
            IF_1 => {
                let flags = self.interrupt.interrupt_flags.read(HalfwordIo::B1);
                let value = (value & flags) ^ flags;
                self.interrupt.interrupt_flags.write(value, HalfwordIo::B1)
            },

            WAITCNT_0 => self.gamepak.registers.waitstate_control.write(value, HalfwordIo::B0),
            WAITCNT_1 => self.gamepak.registers.waitstate_control.write(value, HalfwordIo::B1),

            IME_0 => self.interrupt.master_interrupt.write(value, HalfwordIo::B0),
            IME_1 => self.interrupt.master_interrupt.write(value, HalfwordIo::B1),

            HALTCNT_0 => {
                self.halt_controller.halt_control.write(value);
                let halt_state = Some(self.halt_controller.halt_control.power_down_mode());
                self.halt_controller.state = halt_state;
            }

            _ => ()
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
