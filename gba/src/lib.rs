mod arm;
mod audio;
mod bus;
mod gamepak;
mod io;
mod ppu;

use crate::arm::Arm7tdmi;
use crate::bus::{Bus, BusComponents};
use crate::gamepak::core::GamePak;
use crate::ppu::Ppu;

struct BudgetGba {
    cpu: Arm7tdmi,
    cycles: u64,

    bios_ram: Box<[u8]>,
    wram_256: Box<[u8]>,
    wram_32: Box<[u8]>,

    ppu: Ppu,
    gamepak: GamePak,
}

impl BudgetGba {
    fn new() -> Self {
        let BusComponents {
            mut gamepak,
            mut ppu,
            mut bios_ram,
            mut wram_256,
            mut wram_32,
            mut cycles,
        } = BusComponents::new();

        let cpu = Arm7tdmi::new(&mut Bus {
            gamepak: &mut gamepak,
            ppu: &mut ppu,
            bios_ram: &mut bios_ram,
            wram_256: &mut wram_256,
            wram_32: &mut wram_32,
            cycles: &mut cycles,
        });

        Self {
            cpu,
            gamepak,
            ppu,
            bios_ram,
            wram_256,
            wram_32,
            cycles,
        }
    }
}

fn thing() {
    let mut gba = BudgetGba::new();
    gba.cpu.step(&mut Bus {
        gamepak: &mut gba.gamepak,
        ppu: &mut gba.ppu,
        bios_ram: &mut gba.bios_ram,
        wram_256: &mut gba.wram_256,
        wram_32: &mut gba.wram_32,
        cycles: &mut gba.cycles,
    });
}
