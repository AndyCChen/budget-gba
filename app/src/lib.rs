mod app_core;
mod app_display;

use app_core::GbaCorePlugin;
use app_display::GbaDisplayPlugin;

use bevy::prelude::*;
use gba::{GbaCore, GbaCoreConfig};

#[derive(Resource)]
pub struct Config {
    pub gba_config: GbaCoreConfig,
}

pub struct App;

impl App {
    pub fn run(config: Config) {
        bevy::app::App::new()
            .insert_resource(config)
            .insert_resource(Time::<Fixed>::from_hz(60.0)) // emulation logic will run at a fixed frame rate (60hz)
            .add_plugins(DefaultPlugins)
            .add_plugins(GbaCorePlugin)
            .add_plugins(GbaDisplayPlugin)
            .run();
    }
}

#[derive(States, Clone, Copy, Default, Eq, PartialEq, Hash, Debug)]
enum AppState {
    #[default]
    WaitingForGamepak,
    Running,
    Paused,
}

#[derive(Resource)]
struct BudgetGba(Box<GbaCore>);

impl BudgetGba {
    fn new() -> Self {
        Self(Box::new(GbaCore::new()))
    }
}
