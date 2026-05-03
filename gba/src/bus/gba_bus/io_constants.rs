use paste::paste;

// helper macro to generate halfword io register addresses with
// suffixes _0 and _1 because I'm lazy
macro_rules! io_register_u16 {
    ($base_address:expr, $identifier:tt) => {
        paste! {
            pub const [<$identifier _0>]: usize = $base_address;
        }
        paste! {
            pub const [<$identifier _1>]: usize = $base_address + 1;
        }
    };
}

macro_rules! io_register_u32 {
    ($base_address:expr, $identifier:tt) => {
        paste! {
            pub const [<$identifier _0>]: usize = $base_address;
        }
        paste! {
            pub const [<$identifier _1>]: usize = $base_address + 1;
        }
        paste! {
            pub const [<$identifier _2>]: usize = $base_address + 2;
        }
        paste! {
            pub const [<$identifier _3>]: usize = $base_address + 4;
        }
    };
}

// LCD I/O Registers
io_register_u16!(0x400_0000, DISPCNT);
// io_register_u16!(0x400_0002, GREENSWAP); Not sure what this register does yet
io_register_u16!(0x400_0004, DISPSTAT);
io_register_u16!(0x400_0006, VCOUNT);
io_register_u16!(0x400_0008, BG0CNT);
io_register_u16!(0x400_000A, BG1CNT);
io_register_u16!(0x400_000C, BG2CNT);
io_register_u16!(0x400_000E, BG3CNT);
io_register_u16!(0x400_0010, BG0HOFS);
io_register_u16!(0x400_0012, BG0VOFS);
io_register_u16!(0x400_0014, BG1HOFS);
io_register_u16!(0x400_0016, BG1VOFS);
io_register_u16!(0x400_0018, BG2HOFS);
io_register_u16!(0x400_001A, BG2VOFS);
io_register_u16!(0x400_001C, BG3HOFS);
io_register_u16!(0x400_001E, BG3VOFS);
io_register_u16!(0x400_0020, BG2PA);
io_register_u16!(0x400_0022, BG2PB);
io_register_u16!(0x400_0024, BG2PC);
io_register_u16!(0x400_0026, BG2PD);
io_register_u32!(0x400_0028, BG2X);
io_register_u32!(0x400_002C, BG2Y);
io_register_u16!(0x400_0030, BG3PA);
io_register_u16!(0x400_0032, BG3PB);
io_register_u16!(0x400_0034, BG3PC);
io_register_u16!(0x400_0036, BG3PD);
io_register_u32!(0x400_0038, BG3X);
io_register_u32!(0x400_003C, BG3Y);
io_register_u16!(0x400_0040, WIN0H);
io_register_u16!(0x400_0042, WIN1H);
io_register_u16!(0x400_0044, WIN0V);
io_register_u16!(0x400_0046, WIN1V);
io_register_u16!(0x400_0048, WININ);
io_register_u16!(0x400_004A, WINOUT);
io_register_u16!(0x400_004C, MOSAIC);
io_register_u16!(0x400_0050, BLDCNT);
io_register_u16!(0x400_0052, BLDALPHA);
io_register_u16!(0x400_0054, BLDY);

// Sound Registers
// DMA Transfer Channels

// Timer Registers
io_register_u16!(0x400_0100, TM0CNT_L);
io_register_u16!(0x400_0102, TM0CNT_H);
io_register_u16!(0x400_0104, TM1CNT_L);
io_register_u16!(0x400_0106, TM1CNT_H);
io_register_u16!(0x400_0108, TM2CNT_L);
io_register_u16!(0x400_010A, TM2CNT_H);
io_register_u16!(0x400_010C, TM3CNT_L);
io_register_u16!(0x400_010E, TM3CNT_H);

// Serial Communication (1)

// Keypad Input
io_register_u16!(0x400_0130, KEYINPUT);
io_register_u16!(0x400_0132, KEYCNT);

// Serial Communication (2)

// Watstate, and Power-Down Control
io_register_u16!(0x400_0200, IE);
io_register_u16!(0x400_0202, IF);
io_register_u16!(0x400_0204, WAITCNT);
io_register_u16!(0x400_0208, IME);
