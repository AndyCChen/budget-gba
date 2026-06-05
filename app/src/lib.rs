pub mod config;

use bevy::prelude::*;
pub use config::Config;
use gba::GbaCore;

pub fn start(config: Config) {
    App::new()
        .add_plugins(DefaultPlugins)
        .insert_resource(config)
        .add_systems(Startup, app_init)
        .add_systems(Update, app_loop)
        .run();
}

#[derive(Resource)]
struct BudgetGba {
    gba_core: Box<GbaCore>,
}

impl BudgetGba {
    fn new(config: &Config) -> Self {
        let mut gba_core = GbaCore::new();
        gba_core.load_bios(&config.bios_path);

        if let Some(gamepak_path) = &config.gamepak_path {
            gba_core.load_gamepak(gamepak_path);
        }

        Self {
            gba_core: Box::new(gba_core),
        }
    }
}

fn app_init(mut commands: Commands, config: Res<Config>) {
    let budget_gba = BudgetGba::new(&config);
    commands.insert_resource(budget_gba);
}

fn app_loop(budget_gba: ResMut<BudgetGba>) {}
