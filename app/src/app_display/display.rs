use bevy::{
    asset::RenderAssetUsages,
    image::ImageSampler,
    prelude::*,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat},
};

use crate::BudgetGba;
use crate::app_display::DisplayTexture;
use gba::Rgb5;

pub fn display_setup(mut commands: Commands, mut images: ResMut<Assets<Image>>) {
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

pub fn display_update(
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
