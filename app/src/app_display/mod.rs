mod display;

use bevy::{
    app::{App, Plugin, Startup, Update},
    asset::Handle,
    ecs::resource::Resource,
    image::Image,
};
use display::*;

/// Plugin that handles rendering the main pixel viewport
pub struct GbaDisplayPlugin;

impl Plugin for GbaDisplayPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, display_setup);
        app.add_systems(Update, display_update);
    }
}

#[derive(Resource)]
struct DisplayTexture(Handle<Image>);
