use crate::app_core::gba_single_step;
use crate::{AppState, BudgetGba};
use bevy::prelude::*;
use gba::{GbaKeyCode, KeypadInputType::*};

pub fn handle_app_input(
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

pub fn handle_gba_input(mut gba: ResMut<BudgetGba>, input: Res<ButtonInput<KeyCode>>) {
    for just_released in input.get_just_released() {
        match just_released {
            KeyCode::KeyW => gba.0.keypad_set_input(Released, GbaKeyCode::Up),
            KeyCode::KeyA => gba.0.keypad_set_input(Released, GbaKeyCode::Left),
            KeyCode::KeyS => gba.0.keypad_set_input(Released, GbaKeyCode::Down),
            KeyCode::KeyD => gba.0.keypad_set_input(Released, GbaKeyCode::Right),
            KeyCode::KeyK => gba.0.keypad_set_input(Released, GbaKeyCode::KeyB),
            KeyCode::KeyL => gba.0.keypad_set_input(Released, GbaKeyCode::KeyA),
            KeyCode::KeyE => gba.0.keypad_set_input(Released, GbaKeyCode::Select),
            KeyCode::KeyQ => gba.0.keypad_set_input(Released, GbaKeyCode::Start),
            KeyCode::ShiftLeft => gba.0.keypad_set_input(Released, GbaKeyCode::KeyL),
            KeyCode::ShiftRight => gba.0.keypad_set_input(Released, GbaKeyCode::KeyR),
            _ => (),
        }
    }

    for just_pressed in input.get_just_pressed() {
        match just_pressed {
            KeyCode::KeyW => gba.0.keypad_set_input(Pressed, GbaKeyCode::Up),
            KeyCode::KeyA => gba.0.keypad_set_input(Pressed, GbaKeyCode::Left),
            KeyCode::KeyS => gba.0.keypad_set_input(Pressed, GbaKeyCode::Down),
            KeyCode::KeyD => gba.0.keypad_set_input(Pressed, GbaKeyCode::Right),
            KeyCode::KeyK => gba.0.keypad_set_input(Pressed, GbaKeyCode::KeyB),
            KeyCode::KeyL => gba.0.keypad_set_input(Pressed, GbaKeyCode::KeyA),
            KeyCode::KeyE => gba.0.keypad_set_input(Pressed, GbaKeyCode::Select),
            KeyCode::KeyQ => gba.0.keypad_set_input(Pressed, GbaKeyCode::Start),
            KeyCode::ShiftLeft => gba.0.keypad_set_input(Pressed, GbaKeyCode::KeyL),
            KeyCode::ShiftRight => gba.0.keypad_set_input(Pressed, GbaKeyCode::KeyR),
            _ => (),
        }
    }
}
