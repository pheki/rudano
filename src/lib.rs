//! Rudano, Rust Data Notation, is a data serialization format designed to be as
//! similar as possible to Rust's own syntax.
//!
//! # Usage
//!
//! Add this to your `Cargo.toml` (verifying the version number):
//!
//! ```text
//! [dependencies]
//! rudano = "0.1"
//! ```
//!
//! Then you can use `to_string_pretty` or `to_string_compact` for serialization and `from_str` for
//! deserialization. There are examples in the function docs.

mod array_seq;
pub mod de;
pub mod error;
pub mod ser;

#[doc(inline)]
pub use error::{
    DeserializationError, DeserializationResult, SerializationError, SerializationResult,
};

#[doc(inline)]
pub use de::{from_str, Deserializer};

#[doc(inline)]
pub use ser::{to_string_compact, to_string_pretty, Serializer};

#[doc(inline)]
pub use array_seq::serialize_array;
