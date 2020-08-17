//! Error structures for Rudano.
//!
//! Serialization and Deserialization errors are different types as they do not have much in
//! common, after all.

use std::fmt::{self, Display};
use std::num::NonZeroUsize;

use serde::{de, ser};

/// Result of deserialization operation.
pub type DeserializationResult<T> = std::result::Result<T, DeserializationError>;
/// Result of serialization operation.
pub type SerializationResult<T> = std::result::Result<T, SerializationError>;

/// Information on data deserialization error.
#[derive(Clone, Debug, PartialEq)]
pub struct DeserializationError {
    /// Error code, possibly with related data.
    pub code: DeserializationErrorCode,
    /// Line where the error ocurred.
    pub line: Option<NonZeroUsize>,
    /// Column where the error ocurred.
    pub column: Option<NonZeroUsize>,
}

/// Deserialization Error Code, possibly with related data (such as invalid char).
#[derive(Clone, Debug, PartialEq)]
pub enum DeserializationErrorCode {
    CustomError(String),

    UnexpectedEof,
    UnexpectedTrailingCharacters,

    ExpectedAny,
    ExpectedBoolean(char),
    ExpectedInteger(char),
    IntegerOutOfRange(String),
    InvalidSuffix(String),
    ExpectedFloat(char),
    ExpectedCharStart(char),
    ExpectedChar,
    ExpectedCharEnd(char),
    ExpectedStringStart(char),
    ExpectedEscapeStart(char),
    EscapeInvalid(char),
    HexEscapeInvalid(String),
    UnicodeEscapeInvalid(String),
    ExpectedEscapeStop,
    ExpectedOption(char),
    ExpectedUnitStart(char),
    ExpectedUnitEnd(char),
    ExpectedTupleStart(char),
    ExpectedTupleEnd(char),
    ExpectedIdentifier(char),
    WrongStructName(String, String),
    ExpectedStructOpening(char),
    ExpectedStructEnd(char),
    ExpectedArrayStart(char),
    ExpectedArrayComma(char),
    ExpectedArrayEnd(char),
    ExpectedMapStart(char),
    ExpectedMapEnd(char),
    ExpectedMapStructColon(char),
    ExpectedMapStructComma(char),

    #[doc(hidden)]
    NonExhaustive,
}

impl de::Error for DeserializationError {
    fn custom<T: Display>(msg: T) -> Self {
        DeserializationError {
            code: DeserializationErrorCode::CustomError(msg.to_string()),
            line: None,
            column: None,
        }
    }
}

impl Display for DeserializationError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use DeserializationErrorCode::*;

        if let (Some(line), Some(column)) = (self.line, self.column) {
            formatter.write_fmt(format_args!("error at line {}, column {}: ", line, column))?;
        } else {
            formatter.write_str("error at unknown line and column")?;
        }

        match self.code {
            CustomError(ref msg) => formatter.write_str(msg),
            UnexpectedEof => formatter.write_str("unexpected end of input"),
            UnexpectedTrailingCharacters => formatter.write_str("unexpected trailing characters"),

            ExpectedAny => formatter.write_str("expected any value, found invalid syntax"),
            ExpectedBoolean(c) => {
                formatter.write_fmt(format_args!("expected boolean, found {:?}", c))
            }
            ExpectedInteger(c) => {
                formatter.write_fmt(format_args!("expected integer, found {:?}", c))
            }
            IntegerOutOfRange(ref s) => {
                formatter.write_fmt(format_args!("integer out of range {:?}", s))
            }
            InvalidSuffix(ref s) => {
                formatter.write_fmt(format_args!("invalid number suffix {:?}", s))
            }
            ExpectedFloat(c) => formatter.write_fmt(format_args!("expected float, found {:?}", c)),
            ExpectedCharStart(c) => {
                formatter.write_fmt(format_args!("expected '\\'', found {:?}", c))
            }
            ExpectedChar => formatter.write_str("expected char, found nothing"),
            ExpectedCharEnd(c) => {
                formatter.write_fmt(format_args!("expected '\\'', found {:?}", c))
            }
            ExpectedStringStart(c) => {
                formatter.write_fmt(format_args!("expected '\"', found {:?}", c))
            }
            ExpectedEscapeStart(c) => {
                formatter.write_fmt(format_args!("expected '{{', found {:?}", c))
            }
            EscapeInvalid(c) => {
                formatter.write_fmt(format_args!("expected escape character, found {:?}", c))
            }
            HexEscapeInvalid(ref s) => {
                formatter.write_fmt(format_args!("hex escape invalid {:?}", s))
            }
            UnicodeEscapeInvalid(ref s) => {
                formatter.write_fmt(format_args!("unicode escape invalid {:?}", s))
            }
            ExpectedEscapeStop => formatter.write_str("expected '}}'"),
            ExpectedOption(c) => {
                formatter.write_fmt(format_args!("expected option, found {:?}", c))
            }
            ExpectedUnitStart(c) => {
                formatter.write_fmt(format_args!("expected '(', found {:?}", c))
            }
            ExpectedUnitEnd(c) => formatter.write_fmt(format_args!("expected ')', found {:?}", c)),
            ExpectedTupleStart(c) => {
                formatter.write_fmt(format_args!("expected '(', found {:?}", c))
            }
            ExpectedTupleEnd(c) => formatter.write_fmt(format_args!("expected ')', found {:?}", c)),
            ExpectedIdentifier(c) => {
                formatter.write_fmt(format_args!("expected identifier, found {:?}", c))
            }
            WrongStructName(ref expected, ref found) => formatter.write_fmt(format_args!(
                "wrong struct name, expected {:?}, found {:?}",
                expected, found
            )),
            ExpectedStructOpening(c) => {
                formatter.write_fmt(format_args!("expected '{{', found {:?}", c))
            }
            ExpectedStructEnd(c) => {
                formatter.write_fmt(format_args!("expected '}}', found {:?}", c))
            }
            ExpectedArrayStart(c) => {
                formatter.write_fmt(format_args!("expected '[', found {:?}", c))
            }
            ExpectedArrayComma(c) => {
                formatter.write_fmt(format_args!("expected ',', found {:?}", c))
            }
            ExpectedArrayEnd(c) => formatter.write_fmt(format_args!("expected ']', found {:?}", c)),
            ExpectedMapStart(c) => formatter.write_fmt(format_args!("expected '[', found {:?}", c)),
            ExpectedMapEnd(c) => formatter.write_fmt(format_args!("expected ']', found {:?}", c)),
            ExpectedMapStructColon(c) => {
                formatter.write_fmt(format_args!("expected ':', found {:?}", c))
            }
            ExpectedMapStructComma(c) => {
                formatter.write_fmt(format_args!("expected ',', found {:?}", c))
            }
            NonExhaustive => formatter.write_str("this variant should not be used"),
        }
    }
}

impl std::error::Error for DeserializationError {}

/// All possible errors that may occur when Serializing Rudano.
#[derive(Clone, Debug, PartialEq)]
pub enum SerializationError {
    /// Custom Error. This is usually created by the Serialize implementation of the data being
    /// serialized.
    CustomError(String),
}

impl ser::Error for SerializationError {
    fn custom<T: Display>(msg: T) -> Self {
        SerializationError::CustomError(msg.to_string())
    }
}

impl Display for SerializationError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let msg = match *self {
            SerializationError::CustomError(ref msg) => msg,
        };

        formatter.write_str(msg)
    }
}

impl std::error::Error for SerializationError {}

impl From<fmt::Error> for SerializationError {
    fn from(_err: fmt::Error) -> SerializationError {
        SerializationError::CustomError("could not format number".to_string())
    }
}
