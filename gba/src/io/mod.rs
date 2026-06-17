pub mod constants;

pub enum HalfwordIo {
    B0, // 1st byte
    B1, // 2nd byte
}

#[allow(dead_code)]
pub enum WordIo {
    B0, // 1st byte
    B1, // etc...
    B2,
    B3,
}
