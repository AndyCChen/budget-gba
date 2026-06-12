use crate::{AppState, BudgetGba};
use bevy::prelude::*;

/// Run emulation for a single frame (until vblank).
pub fn gba_frame_step(mut gba: ResMut<BudgetGba>) {
    info_once!("GBA RUN");

    // sync to vblank for now
    while !gba.0.is_frame_complete() {
        gba.0.step();
    }
}

/// Single step a single cpu instruction
pub fn gba_single_step(mut gba: ResMut<BudgetGba>) {
    gba.0.toggle_cpu_log(true);
    gba.0.step();
    gba.0.print_cpu_log();
}

pub fn _gba_waiting(mut _app_state: ResMut<NextState<AppState>>) {
    info_once!("GBA WAIT");
}
