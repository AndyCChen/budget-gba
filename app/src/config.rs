use bevy::ecs::resource::Resource;
use gba::GbaCoreConfig;

#[derive(Resource)]
pub struct Config {
    pub gba_config: GbaCoreConfig,
}
