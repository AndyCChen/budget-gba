mod arm;
mod bus;
mod ppu;
mod io;

use godot::prelude::*;

use crate::arm::Arm7tdmi;

struct GbaExtension;

#[gdextension]
unsafe impl ExtensionLibrary for GbaExtension {}

#[derive(GodotClass)]
struct BudgetGba {
    cpu: Arm7tdmi,

    base: Base<RefCounted>,
}

#[godot_api]
impl IRefCounted for BudgetGba {
    fn init(base: Base<RefCounted>) -> Self {
        godot_print!("GBA init from rust!");
        Self {
            cpu: Arm7tdmi::new(),
            base,
        }
    }
}

#[godot_api]
impl BudgetGba {
    #[func]
    fn on_update(&mut self, delta: f64) {
        // run the emulation for 1 frame here
        godot_print!("{delta}");
    }

    #[func]
    fn reset(&mut self) {
        self.cpu.reset();
    }
}
