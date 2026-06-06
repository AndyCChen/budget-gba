pub mod config;

use bevy::prelude::*;
pub use config::Config;
use gba::{GbaCore, GbaError::*};

#[derive(States, Clone, Copy, Default, Eq, PartialEq, Hash, Debug)]
enum AppState {
    #[default]
    WaitingForGamepak,
    Running,
    Paused,
}

pub struct BudgetGba {
    gba_core: Box<GbaCore>,
}

impl BudgetGba {
    fn new() -> Self {
        Self {
            gba_core: Box::new(GbaCore::new()),
        }
    }

    pub fn start(config: Config) {
        App::new()
            .add_plugins(DefaultPlugins)
            .insert_resource(config)
            .insert_non_send_resource(BudgetGba::new())
            .init_state::<AppState>()
            .add_systems(Startup, app_init)
            .add_systems(
                Update,
                (
                    gba_waiting.run_if(in_state(AppState::WaitingForGamepak)),
                    gba_running.run_if(in_state(AppState::Running)),
                    gba_paused.run_if(in_state(AppState::Paused)),
                ),
            )
            .run();
    }
}

fn app_init(
    mut budget_gba: NonSendMut<BudgetGba>,
    config: Res<Config>,
    mut exit: MessageWriter<AppExit>,
    mut app_state: ResMut<NextState<AppState>>,
) {
    match budget_gba.gba_core.load_config(&config.gba_config) {
        Ok(_) => app_state.set(if config.gba_config.gamepak_path.is_none() {
            AppState::WaitingForGamepak
        } else {
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

fn gba_running(
    _budget_gba: NonSendMut<BudgetGba>,
    input: Res<ButtonInput<KeyCode>>,
    mut app_state: ResMut<NextState<AppState>>,
) {
    if input.just_released(KeyCode::KeyP) {
        app_state.set(AppState::Paused);
    }
    info_once!("GBA RUN");
}

fn gba_paused(input: Res<ButtonInput<KeyCode>>, mut app_state: ResMut<NextState<AppState>>) {
    if input.just_released(KeyCode::KeyP) {
        app_state.set(AppState::Running);
    }
    info_once!("GBA PAUSE");
}

fn gba_waiting(mut _app_state: ResMut<NextState<AppState>>) {
    info_once!("GBA WAIT");
}
