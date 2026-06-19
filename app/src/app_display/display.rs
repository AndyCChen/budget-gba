use bevy::{
    asset::RenderAssetUsages,
    image::ImageSampler,
    prelude::*,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat},
    window::WindowResized,
};

use crate::{BudgetGba, Config};
use gba::Rgb5;

#[derive(Component)]
pub(crate) struct DisplaySprite;

pub fn window_setup(config: Res<Config>, mut window: Single<&mut Window>) {
    window
        .resolution
        .set(config.window_size.x, config.window_size.y);
}

pub fn window_resize(
    mut sprite: Single<&mut Sprite, With<DisplaySprite>>,
    mut resize_reader: MessageReader<WindowResized>,
) {
    for window_size in resize_reader.read().map(|e| Vec2::new(e.width, e.height)) {
        const WIDTH: f32 = gba::DISPLAY_WIDTH as f32;
        const HEIGHT: f32 = gba::DISPLAY_HEIGHT as f32;

        let width_ratio = window_size.x / WIDTH;
        let height_ratio = window_size.y / HEIGHT;

        // if width_ratio < height_ratio, width is used to calculate new height
        let new_size = if width_ratio < height_ratio {
            // every 3 horizontal pixels == 2 vertical pixels
            let new_height = window_size.x / 3.0 * 2.0;
            let new_width = window_size.x;
            Vec2::new(new_width, new_height)
        } else {
            // every 2 vertical pixels == 3 horizontal pixels
            let new_width = window_size.y / 2.0 * 3.0;
            let new_height = window_size.y;
            Vec2::new(new_width, new_height)
        };

        sprite.custom_size = Some(new_size);
    }
}

pub fn display_setup(
    mut commands: Commands,
    config: Res<Config>,
    mut images: ResMut<Assets<Image>>,
) {
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

    let mut sprite = Sprite::from_image(images.add(image));
    sprite.custom_size = Some(Vec2 {
        x: config.window_size.x,
        y: config.window_size.y,
    });

    commands.spawn((DisplaySprite, sprite));
}

pub fn display_update(
    gba: Res<BudgetGba>,
    sprite: Single<&Sprite, With<DisplaySprite>>,
    mut images: ResMut<Assets<Image>>,
) {
    let display_texture = images
        .get_mut(&sprite.image)
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
