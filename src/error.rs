use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Failed to parse config file {path}: {message}")]
    ConfigParse { path: PathBuf, message: String },

    #[error("No Rust source files found in {0}")]
    NoSourceFiles(PathBuf),

    #[error("{0}")]
    Other(String),
}

pub type Result<T> = std::result::Result<T, Error>;
