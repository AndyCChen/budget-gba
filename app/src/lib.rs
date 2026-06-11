pub mod config;

use bevy::{
    asset::RenderAssetUsages,
    image::ImageSampler,
    prelude::*,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat},
};
pub use config::Config;
use gba::{DISPLAY_HEIGHT, DISPLAY_WIDTH, GbaCore, GbaError::*, Rgb5};

#[derive(States, Clone, Copy, Default, Eq, PartialEq, Hash, Debug)]
enum AppState {
    #[default]
    WaitingForGamepak,
    Running,
    Paused,
}

#[derive(Resource)]
pub struct BudgetGba(Box<GbaCore>);

#[derive(Resource)]
pub struct DisplayTexture(Handle<Image>);

impl BudgetGba {
    fn new() -> Self {
        Self(Box::new(GbaCore::new()))
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
                    gba_running.run_if(in_state(AppState::Running)),
                    update_display,
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

    match gba.0.load_config(&config.gba_config) {
        Ok(_) => app_state.set(if config.gba_config.gamepak_path.is_none() {
            AppState::WaitingForGamepak
        } else {
            gba.0.cpu_pipeline_fill();
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

    let color = Rgb5::black().to_rgba8_array();
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

fn update_display(
    gba: Res<BudgetGba>,
    display_handle: Res<DisplayTexture>,
    mut images: ResMut<Assets<Image>>,
) {
    let display_texture = images
        .get_mut(&display_handle.0)
        .expect("Failed to retrieve display texture!");

    let pixel_data = display_texture.data.as_mut().unwrap();

    let (pixel_data_chunk, remainder) = pixel_data.as_chunks_mut::<4>();
    debug_assert!(remainder.is_empty());
    debug_assert_eq!(
        pixel_data_chunk.len(),
        gba.0.get_display_buffer().as_flattened().len()
    );

    let gba_display_buffer = gba.0.get_display_buffer().iter().flatten();
    for (src, dst) in gba_display_buffer.zip(pixel_data_chunk) {
        dst.copy_from_slice(&src.to_rgba8_array());
    }
}

fn gba_running(mut gba: ResMut<BudgetGba>) {
    info_once!("GBA RUN");

    for _ in 0..10_000 {
        gba.0.step();
    }
}

fn gba_paused(mut gba: ResMut<BudgetGba>, input: Res<ButtonInput<KeyCode>>) {
    info_once!("GBA PAUSE");

    if input.just_released(KeyCode::Space) {
        gba.0.toggle_cpu_log(true);
        gba.0.step();
        gba.0.print_cpu_log();
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
