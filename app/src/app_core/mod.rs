mod gba_runner;
mod gba_startup;
mod inputs;

use bevy::{
    app::{FixedUpdate, Plugin, Startup, Update},
    ecs::schedule::IntoScheduleConfigs,
    state::{app::AppExtStates, condition::in_state},
};

use crate::{AppState, BudgetGba};
use gba_runner::*;
use gba_startup::*;
use inputs::*;

pub struct GbaCorePlugin;

impl Plugin for GbaCorePlugin {
    fn build(&self, app: &mut bevy::app::App) {
        app.init_state::<AppState>();
        app.insert_resource(BudgetGba::new());
        app.add_systems(Startup, gba_setup);
        app.add_systems(
            FixedUpdate,
            gba_frame_step.run_if(in_state(AppState::Running)),
        );
        app.add_systems(Update, (handle_app_input, handle_gba_input));
    }
}
