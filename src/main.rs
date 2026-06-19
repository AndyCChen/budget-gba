use app::*;
use clap::Parser;
use gba::GbaCoreConfig;
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about)]
struct Cli {
    /// Path to bios file (.bin)
    #[arg(short, value_name = "FILE")]
    bios_path: Option<PathBuf>,

    /// Path to gamepak file (.gba)
    #[arg(short, value_name = "FILE")]
    gamepak_path: Option<PathBuf>,

    /// Start emulator in paused state
    #[arg(short, action)]
    paused: bool,
}

fn main() {
    let args = Cli::parse();

    let gba_config = GbaCoreConfig {
        bios_path: PathBuf::from("resource/gba_bios.bin"),
        gamepak_path: args.gamepak_path,
    };

    let config = Config {
        gba_config,
        paused: args.paused,
        window_size: Vec2 {
            x: gba::DISPLAY_WIDTH as f32 * 2.,
            y: gba::DISPLAY_HEIGHT as f32 * 2.,
        },
    };

    App::run(config);
}
