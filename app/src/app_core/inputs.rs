use crate::AppState;
use crate::app_core::gba_single_step;
use bevy::prelude::*;

pub fn handle_inputs(
    mut commands: Commands,
    input: Res<ButtonInput<KeyCode>>,
    current_state: Res<State<AppState>>,
    mut next_state: ResMut<NextState<AppState>>,
) {
    if input.just_released(KeyCode::KeyP) {
        match current_state.get() {
            AppState::Running => next_state.set(AppState::Paused),
            AppState::Paused => next_state.set(AppState::Running),
            _ => (),
        }
    }

    if input.just_released(KeyCode::Space) && matches!(current_state.get(), AppState::Paused) {
        commands.run_system_cached(gba_single_step);
    }
}
