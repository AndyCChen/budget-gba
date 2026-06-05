use app;
use std::path::PathBuf;

fn main() {
    let config = app::Config {
        bios_path: PathBuf::from(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/resource/gba_bios.bin"
        )),
        gamepak_path: None,
    };
    app::start(config);
}
