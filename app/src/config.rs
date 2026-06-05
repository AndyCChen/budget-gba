use std::path::PathBuf;

use bevy::ecs::resource::Resource;

#[derive(Resource)]
pub struct Config {
    pub bios_path: PathBuf,
    pub gamepak_path: Option<PathBuf>,
}
