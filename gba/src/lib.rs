mod arm;
mod bus;
mod gamepak;
mod io;
mod ppu;

use godot::prelude::*;

use crate::arm::{ARM7TDMI_CLOCK_RATE, Arm7tdmi};

struct GbaExtension;

#[gdextension]
unsafe impl ExtensionLibrary for GbaExtension {}

#[derive(GodotClass)]
struct BudgetGba {
    cpu: Arm7tdmi,

    per_frame_cycle_counter: f32,
    base: Base<RefCounted>,
}

#[godot_api]
impl IRefCounted for BudgetGba {
    fn init(base: Base<RefCounted>) -> Self {
        godot_print!("GBA init from rust!");
        Self {
            cpu: Arm7tdmi::new(),
            per_frame_cycle_counter: 0.0,
            base,
        }
    }
}

#[godot_api]
impl BudgetGba {
    #[func]
    // must be called 60 times per second
    fn on_update(&mut self, _delta: f64) {
        const CYCLES_PER_FRAME: f32 = ARM7TDMI_CLOCK_RATE as f32 / 60.0;
        let start_timestamp = self.cpu.bus.cycles();

        while self.per_frame_cycle_counter < CYCLES_PER_FRAME {
            self.cpu.step();
            self.per_frame_cycle_counter += (self.cpu.bus.cycles() - start_timestamp) as f32;
        }

        self.per_frame_cycle_counter -= CYCLES_PER_FRAME;
    }

    #[func]
    fn reset(&mut self) {
        self.cpu.reset();
        self.per_frame_cycle_counter = 0.0;
    }
}
