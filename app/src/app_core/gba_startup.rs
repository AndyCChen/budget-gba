use crate::{AppState, BudgetGba, Config};
use bevy::prelude::*;
use gba::GbaError::*;

pub fn gba_setup(
    mut gba: ResMut<BudgetGba>,
    config: Res<Config>,
    mut exit: MessageWriter<AppExit>,
    mut app_state: ResMut<NextState<AppState>>,
) {
    info!("App Initialization");

    match gba.0.load_config(&config.gba_config) {
        Ok(_) => app_state.set(if config.gba_config.gamepak_path.is_none() {
            AppState::WaitingForGamepak
        } else {
            gba.0.cpu_pipeline_fill();
            AppState::Running
        }),
        Err(e) => match e {
            GamepakLoadFail(e) => {
                app_state.set(AppState::WaitingForGamepak);
                warn!("{}", e.to_string());
            }
            BiosLoadFail(e) => {
                error!("{}", e.to_string());
                exit.write(AppExit::Success);
            }
        },
    }
}
