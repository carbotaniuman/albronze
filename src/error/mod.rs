mod handler;

mod warning;

pub use warning::*;

pub type ErrorHandler<T> = handler::ErrorHandler<T, Warning>;
