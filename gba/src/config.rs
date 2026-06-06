use std::path::PathBuf;

pub struct GbaCoreConfig {
    pub bios_path: PathBuf,
    pub gamepak_path: Option<PathBuf>,
}
