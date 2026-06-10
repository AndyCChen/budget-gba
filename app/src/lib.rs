pub mod config;

use bevy::prelude::*;
pub use config::Config;
use gba::{ARM7TDMI_CLOCK_RATE, GbaCore, GbaError::*};

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
            .insert_resource(Time::<Fixed>::from_hz(60.0))
            .insert_non_send_resource(BudgetGba::new())
            .init_state::<AppState>()
            .add_systems(Startup, app_init)
            .add_systems(FixedUpdate, gba_running.run_if(in_state(AppState::Running)))
            .add_systems(
                Update,
                (
                    gba_waiting.run_if(in_state(AppState::WaitingForGamepak)),
                    gba_paused.run_if(in_state(AppState::Paused)),
                    handle_inputs,
                ),
            )
            .run();
    }
}

fn app_init(
    mut gba: NonSendMut<BudgetGba>,
    config: Res<Config>,
    mut exit: MessageWriter<AppExit>,
    mut app_state: ResMut<NextState<AppState>>,
) {
    match gba.gba_core.load_config(&config.gba_config) {
        Ok(_) => app_state.set(if config.gba_config.gamepak_path.is_none() {
            AppState::WaitingForGamepak
        } else {
            gba.gba_core.cpu_pipeline_fill();
            // gba.gba_core.toggle_cpu_log(true);
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

fn gba_running(mut gba: NonSendMut<BudgetGba>) {
    info_once!("GBA RUN");

    for _ in 0..10_000 {
        gba.gba_core.step();
    }

    // gba.gba_core.print_cpu_log();
}

fn gba_paused(mut gba: NonSendMut<BudgetGba>, input: Res<ButtonInput<KeyCode>>) {
    info_once!("GBA PAUSE");

    if input.just_released(KeyCode::Space) {
        // gba.gba_core.step();
        // gba.gba_core.print_cpu_log();
    }
}

fn gba_waiting(mut _app_state: ResMut<NextState<AppState>>) {
    info_once!("GBA WAIT");
}

fn handle_inputs(
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
}
