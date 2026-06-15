use app::*;
use clap::Parser;
use gba::GbaCoreConfig;
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about)]
struct Args {
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
    let args = Args::parse();
    println!("{}", args.paused);

    let gba_config = GbaCoreConfig {
        bios_path: PathBuf::from("resource/gba_bios.bin"),
        gamepak_path: args.gamepak_path,
    };

    let config = Config {
        gba_config,
        paused: args.paused,
    };

    App::run(config);
}
