pub mod config;

use bevy::prelude::*;
pub use config::Config;
use gba::GbaCore;

pub fn start(config: Config) {
    let budget_gba = BudgetGba::new(&config);

    App::new()
        .add_plugins(DefaultPlugins)
        .insert_resource(config)
        .insert_non_send_resource(budget_gba)
        .add_systems(Update, app_loop)
        .run();
}

struct BudgetGba {
    gba_core: Box<GbaCore>,
}

impl BudgetGba {
    fn new(config: &Config) -> Self {
        let mut gba_core = Box::new(GbaCore::new());
        gba_core.load_bios(&config.bios_path);

        if let Some(gamepak_path) = &config.gamepak_path {
            gba_core.load_gamepak(gamepak_path);
        }

        Self { gba_core }
    }
}

fn app_loop(budget_gba: NonSendMut<BudgetGba>, config: Res<Config>) {
    println!("{:?}", config.bios_path);
}
