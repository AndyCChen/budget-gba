pub mod config;

use bevy::prelude::*;
pub use config::Config;
use gba::{GbaCore, GbaError::*};

pub fn start(config: Config) {
    App::new()
        .add_plugins(DefaultPlugins)
        .insert_resource(config)
        .insert_non_send_resource(BudgetGba::new())
        .add_systems(Startup, app_init)
        .add_systems(Update, app_loop)
        .run();
}

struct BudgetGba {
    gba_core: Box<GbaCore>,
}

impl BudgetGba {
    fn new() -> Self {
        Self {
            gba_core: Box::new(GbaCore::new()),
        }
    }
}

fn app_init(
    mut budget_gba: NonSendMut<BudgetGba>,
    config: Res<Config>,
    mut exit: MessageWriter<AppExit>,
) {
    match budget_gba.gba_core.load_config(&config.gba_config) {
        Ok(_) => (),
        Err(e) => match e {
            GamepakLoadFail(e) => warn!("{}", e.to_string()),
            BiosLoadFail(e) => {
                error!("{}", e.to_string());
                exit.write(AppExit::Success);
            }
        },
    }
}

fn app_loop(
    _budget_gba: NonSendMut<BudgetGba>,
    config: Res<Config>,
    input: Res<ButtonInput<KeyCode>>,
) {
    if input.just_released(KeyCode::KeyP) {
        println!("{:?}", config.gba_config.bios_path);
    }
}
