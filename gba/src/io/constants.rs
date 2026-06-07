use paste::paste;

/// helper macro to generate halfword io register addresses with
/// suffixes _0 and _1 because I'm lazy
macro_rules! io_register_u16 {
    ($identifier:tt, $base_address:expr) => {
        paste! {
            pub const [<$identifier _0>]: usize = $base_address;
        }
        paste! {
            pub const [<$identifier _1>]: usize = $base_address + 1;
        }
    };
}

#[allow(unused_macros)]
macro_rules! io_register_u32 {
    ($identifier:tt, $base_address:expr) => {
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
            pub const [<$identifier _3>]: usize = $base_address + 3;
        }
    };
}

// LCD I/O Registers
io_register_u16!(DISPCNT, 0x400_0000);
//io_register_u16!(GREENSWAP, 0x400_0002); // Not sure what this register does yet
io_register_u16!(DISPSTAT, 0x400_0004);
io_register_u16!(VCOUNT, 0x400_0006);
io_register_u16!(BG0CNT, 0x400_0008);
// io_register_u16!(BG1CNT, 0x400_000A);
// io_register_u16!(BG2CNT, 0x400_000C);
// io_register_u16!(BG3CNT, 0x400_000E);
// io_register_u16!(BG0HOFS, 0x400_0010);
// io_register_u16!(BG0VOFS, 0x400_0012);
// io_register_u16!(BG1HOFS, 0x400_0014);
// io_register_u16!(BG1VOFS, 0x400_0016);
// io_register_u16!(BG2HOFS, 0x400_0018);
// io_register_u16!(BG2VOFS, 0x400_001A);
// io_register_u16!(BG3HOFS, 0x400_001C);
// io_register_u16!(BG3VOFS, 0x400_001E);
// io_register_u16!(BG2PA, 0x400_0020);
// io_register_u16!(BG2PB, 0x400_0022);
// io_register_u16!(BG2PC, 0x400_0024);
// io_register_u16!(BG2PD, 0x400_0026);
// io_register_u32!(BG2X, 0x400_0028);
// io_register_u32!(BG2Y, 0x400_002C);
// io_register_u16!(BG3PA, 0x400_0030);
// io_register_u16!(BG3PB, 0x400_0032);
// io_register_u16!(BG3PC, 0x400_0034);
// io_register_u16!(BG3PD, 0x400_0036);
// io_register_u32!(BG3X, 0x400_0038);
// io_register_u32!(BG3Y, 0x400_003C);
// io_register_u16!(WIN0H, 0x400_0040);
// io_register_u16!(WIN1H, 0x400_0042);
// io_register_u16!(WIN0V, 0x400_0044);
// io_register_u16!(WIN1V, 0x400_0046);
// io_register_u16!(WININ, 0x400_0048);
// io_register_u16!(WINOUT, 0x400_004A);
// io_register_u16!(MOSAIC, 0x400_004C);
// io_register_u16!(BLDCNT, 0x400_0050);
// io_register_u16!(BLDALPHA, 0x400_0052);
// io_register_u16!(BLDY, 0x400_0054);

// Sound Registers
// io_register_u16!(SOUND1CNT_L, 0x400_0060);
// io_register_u16!(SOUND1CNT_H, 0x400_0062);
// io_register_u16!(SOUND1CNT_X, 0x400_0064);
// io_register_u16!(SOUND2CNT_L, 0x400_0068);
// io_register_u16!(SOUND2CNT_H, 0x400_006C);
// io_register_u16!(SOUND3CNT_L, 0x400_0070);
// io_register_u16!(SOUND3CNT_H, 0x400_0072);
// io_register_u16!(SOUND3CNT_X, 0x400_0074);
// io_register_u16!(SOUND4CNT_L, 0x400_0078);
// io_register_u16!(SOUND4CNT_H, 0x400_007C);
// io_register_u16!(SOUNDCNT_L, 0x400_0080);
// io_register_u16!(SOUNDCNT_H, 0x400_0082);
// io_register_u16!(SOUNDCNT_X, 0x400_0084);
io_register_u32!(SOUNDBIAS, 0x400_0088);
// io_register_u16!(WAVE_RAM, 0x400_0090);
// io_register_u32!(FIFO_A, 0x400_00A0);
// io_register_u32!(FIFO_B, 0x400_00A4);

// DMA Transfer Channels
// io_register_u32!(DMA0SAD, 0x400_00B0);
// io_register_u32!(DMA0DAD, 0x400_00B4);
// io_register_u16!(DMA0CNT_L, 0x400_00B8);
// io_register_u16!(DMA0CNT_H, 0x400_00BA);

// io_register_u32!(DMA1SAD, 0x400_00BC);
// io_register_u32!(DMA1DAD, 0x400_00C0);
// io_register_u16!(DMA1CNT_L, 0x400_00C4);
// io_register_u16!(DMA1CNT_H, 0x400_00C6);

// io_register_u32!(DMA2SAD, 0x400_00C8);
// io_register_u32!(DMA2DAD, 0x400_00CC);
// io_register_u16!(DMA2CNT_L, 0x400_00D0);
// io_register_u16!(DMA2CNT_H, 0x400_00D2);

// io_register_u32!(DMA3SAD, 0x400_00D4);
// io_register_u32!(DMA3DAD, 0x400_00D8);
// io_register_u16!(DMA3CNT_L, 0x400_00DC);
// io_register_u16!(DMA3CNT_H, 0x400_00DE);

// Timer Registers
// io_register_u16!(TM0CNT_L, 0x400_0100);
// io_register_u16!(TM0CNT_H, 0x400_0102);
// io_register_u16!(TM1CNT_L, 0x400_0104);
// io_register_u16!(TM1CNT_H, 0x400_0106);
// io_register_u16!(TM2CNT_L, 0x400_0108);
// io_register_u16!(TM2CNT_H, 0x400_010A);
// io_register_u16!(TM3CNT_L, 0x400_010C);
// io_register_u16!(TM3CNT_H, 0x400_010E);

// Serial Communication (1)

// Keypad Input
io_register_u16!(KEYINPUT, 0x400_0130);
io_register_u16!(KEYCNT, 0x400_0132);

// Serial Communication (2)

// Watstate, and Power-Down Control
// io_register_u16!(IE, 0x400_0200);
// io_register_u16!(IF, 0x400_0202);
io_register_u32!(WAITCNT, 0x400_0204);
// io_register_u16!(IME, 0x400_0208);
