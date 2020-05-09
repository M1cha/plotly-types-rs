use error_rules::*;

#[derive(Debug, Error)]
pub enum Error {
    #[error_from]
    Json(serde_json::Error),
    #[error_from]
    Io(std::io::Error),
}
