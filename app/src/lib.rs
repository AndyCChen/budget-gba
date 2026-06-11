pub mod config;

use bevy::{
    asset::RenderAssetUsages,
    image::ImageSampler,
    prelude::*,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat},
};
pub use config::Config;
use gba::{GbaCore, GbaError::*, Rgb5};

#[derive(States, Clone, Copy, Default, Eq, PartialEq, Hash, Debug)]
enum AppState {
    #[default]
    WaitingForGamepak,
    Running,
    Paused,
}

#[derive(Resource)]
pub struct BudgetGba {
    gba_core: Box<GbaCore>,
}

#[derive(Resource)]
pub struct DisplayTexture(Handle<Image>);

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
            .insert_resource(BudgetGba::new())
            .init_state::<AppState>()
            .add_systems(Startup, (app_init, setup_display))
            .add_systems(
                FixedUpdate,
                (
                    update_display,
                    gba_running.run_if(in_state(AppState::Running)),
                ),
            )
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
    mut gba: ResMut<BudgetGba>,
    config: Res<Config>,
    mut exit: MessageWriter<AppExit>,
    mut app_state: ResMut<NextState<AppState>>,
) {
    info!("App Initialization");

    match gba.gba_core.load_config(&config.gba_config) {
        Ok(_) => app_state.set(if config.gba_config.gamepak_path.is_none() {
            AppState::WaitingForGamepak
        } else {
            gba.gba_core.cpu_pipeline_fill();
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

fn setup_display(mut commands: Commands, mut images: ResMut<Assets<Image>>) {
    info!("Display Initialization");
    commands.spawn(Camera2d);

    let color = Rgb5::white().to_rgba8_array();
    let mut image = Image::new_fill(
        Extent3d {
            width: gba::DISPLAY_WIDTH as u32,
            height: gba::DISPLAY_HEIGHT as u32,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        &color,
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
    );
    image.sampler = ImageSampler::nearest();

    let handle = images.add(image);
    commands.insert_resource(DisplayTexture(handle.clone()));

    let mut sprite = Sprite::from_image(handle);
    sprite.custom_size = Some(Vec2 {
        x: 3.0 * gba::DISPLAY_WIDTH as f32,
        y: 3.0 * gba::DISPLAY_HEIGHT as f32,
    });
    commands.spawn(sprite);
}

fn update_display(display_handle: Res<DisplayTexture>, mut images: ResMut<Assets<Image>>) {
    let color = Rgb5::new().with_red(31).to_rgba8_array();
    let color = Srgba::rgba_u8(color[0], color[1], color[2], color[3]);

    let display_texture = images
        .get_mut(&display_handle.0)
        .expect("Failed to retrieve display texture!");

    display_texture
        .set_color_at(100, 100, Color::Srgba(color))
        .unwrap();
}

fn gba_running(mut gba: ResMut<BudgetGba>) {
    info_once!("GBA RUN");

    for _ in 0..10_000 {
        gba.gba_core.step();
    }
}

fn gba_paused(mut gba: ResMut<BudgetGba>, input: Res<ButtonInput<KeyCode>>) {
    info_once!("GBA PAUSE");

    if input.just_released(KeyCode::Space) {
        gba.gba_core.toggle_cpu_log(true);
        gba.gba_core.step();
        gba.gba_core.print_cpu_log();
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
