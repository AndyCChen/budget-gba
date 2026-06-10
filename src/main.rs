use app::*;
use gba::GbaCoreConfig;
use std::path::PathBuf;

fn main() {
    let gba_config = GbaCoreConfig {
        bios_path: PathBuf::from(format!(
            "resource/gba_bios.bin",
        )),
        gamepak_path: Some(PathBuf::from("resource/first.gba")),
    };

    let config = Config { gba_config };
    BudgetGba::start(config);
}
