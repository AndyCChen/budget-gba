use app;
use gba::GbaCoreConfig;
use std::path::PathBuf;

fn main() {
    let gba_config = GbaCoreConfig {
        bios_path: PathBuf::from(format!(
            "{}/resource/gba_bios.bin",
            env!("CARGO_MANIFEST_DIR")
        )),
        gamepak_path: None,
    };

    let config = app::Config { gba_config };
    app::start(config);
}
