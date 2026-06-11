mod apu;
mod arm;
mod bus;
mod common;
mod config;
mod core;
mod gamepak;
mod io;
mod keypad;
mod ppu;
mod scheduler;

pub use arm::ARM7TDMI_CLOCK_RATE;
pub use common::{DISPLAY_HEIGHT, DISPLAY_WIDTH, Rgb5};
pub use config::GbaCoreConfig;
pub use core::*;
