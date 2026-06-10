mod apu;
mod arm;
mod bus;
mod config;
mod core;
mod gamepak;
mod io;
mod keypad;
mod ppu;
mod scheduler;
mod common;

pub use config::GbaCoreConfig;
pub use core::*;
pub use arm::ARM7TDMI_CLOCK_RATE;
