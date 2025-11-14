// LCD I/O Registers
pub const DISPCNT: usize = 0x400_0000;
pub const DISPSTAT: usize = 0x400_0004;
pub const VCOUNT: usize = 0x400_0006;
pub const BG0CNT: usize = 0x400_0008;
pub const BG1CNT: usize = 0x400_000A;
pub const BG2CNT: usize = 0x400_000C;
pub const BG3CNT: usize = 0x400_000E;
pub const BG0HOFS: usize = 0x400_0010;
pub const BG0VOFS: usize = 0x400_0012;
pub const BG1HOFS: usize = 0x400_0014;
pub const BG1VOFS: usize = 0x400_0016;
pub const BG2HOFS: usize = 0x400_0018;
pub const BG2VOFS: usize = 0x400_001A;
pub const BG3HOFS: usize = 0x400_001C;
pub const BG3VOFS: usize = 0x400_001E;
pub const BG2PA: usize = 0x400_0020;
pub const BG2PB: usize = 0x400_0022;
pub const BG2PC: usize = 0x400_0024;
pub const BG2PD: usize = 0x400_0026;
pub const BG2X: usize = 0x400_0028;
pub const BG2Y: usize = 0x400_002C;
pub const BG3PA: usize = 0x400_0030;
pub const BG3PB: usize = 0x400_0032;
pub const BG3PC: usize = 0x400_0034;
pub const BG3PD: usize = 0x400_0036;
pub const BG3X: usize = 0x400_0038;
pub const BG3Y: usize = 0x400_003C;
pub const WIN0H: usize = 0x400_0040;
pub const WIN1H: usize = 0x400_0042;
pub const WIN0V: usize = 0x400_0044;
pub const WIN1V: usize = 0x400_0046;
pub const WININ: usize = 0x400_0048;
pub const WINOUT: usize = 0x400_004A;
pub const MOSAIC: usize = 0x400_004C;
pub const BLDCNT: usize = 0x400_0050;
pub const BLDALPHA: usize = 0x400_0052;
pub const BLDY: usize = 0x400_0054;

// Sound Registers
// DMA Transfer Channels

// Timer Registers
const TM0CNT_L: usize = 0x400_0100;
const TM0CNT_H: usize = 0x400_0102;
const TM1CNT_L: usize = 0x400_0104;
const TM1CNT_H: usize = 0x400_0106;
const TM2CNT_L: usize = 0x400_0108;
const TM2CNT_H: usize = 0x400_010A;
const TM3CNT_L: usize = 0x400_010C;
const TM3CNT_H: usize = 0x400_010E;

// Serial Communication (1)

// Keypad Input
const KEYINPUT: usize = 0x400_0130;
const KEYCNT: usize = 0x400_0132;

// Serial Communication (2)

// Watstate, and Power-Down Control
const IE: usize = 0x400_0200;
const IF: usize = 0x400_0202;
const WAITCNT: usize = 0x400_0204;
const IME: usize = 0x400_0208;
