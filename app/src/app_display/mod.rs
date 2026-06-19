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
        app.add_systems(Startup, (window_setup, display_setup));
        app.add_systems(Update, (window_resize, display_update));
    }
}

#[derive(Resource)]
struct DisplayTexture(Handle<Image>);
