//! Utilities for deserializing data structures from Rudano.

use std::borrow::Cow;
use std::num::NonZeroUsize;
use std::str;

use serde::de::{
    self, DeserializeSeed, EnumAccess, IgnoredAny, MapAccess, SeqAccess, VariantAccess, Visitor,
};
use serde::Deserialize;

use num_traits::cast::FromPrimitive;
use num_traits::ops::checked::{CheckedAdd, CheckedMul, CheckedSub};
use num_traits::{Num, Zero};

use crate::error::{
    DeserializationError as Error, DeserializationErrorCode as ErrorCode,
    DeserializationResult as Result,
};

fn subslice_offset(outer: &str, inner: &str) -> usize {
    inner.as_ptr() as usize - outer.as_ptr() as usize
}

enum ParseCharResult {
    Delimiter,
    NormalChar(char),
    EscapedChar(char),
}

impl ParseCharResult {
    fn char(&self) -> Option<char> {
        use ParseCharResult::*;
        match self {
            Delimiter => None,
            NormalChar(c) => Some(*c),
            EscapedChar(c) => Some(*c),
        }
    }
}

enum Integer {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

/// Deserializes the given `str` as a value of type `T`.
///
/// # Example
///
/// ```
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// # #[derive(PartialEq, Debug)]
/// struct MyStruct {
///     number: i32,
///     string: String,
/// }
///
/// let string = r#"MyStruct {
///     number: 69,
///     string: "nice",
/// }"#;
/// let value: MyStruct = rudano::from_str(string).unwrap();
///
/// let expected = MyStruct {
///     number: 69,
///     string: "nice".to_string(),
/// };
///
/// assert_eq!(value, expected);
/// ```
///
/// # Errors
///
/// Deserialization can fail if `T`'s Deserialize fail, the input is invalid Rudano or the data
/// layout doesn't match.
pub fn from_str<'a, T: Deserialize<'a>>(s: &'a str) -> Result<T> {
    let mut deserializer = Deserializer::from_str(s);
    let t = T::deserialize(&mut deserializer)?;
    deserializer.skip_whitespace()?;
    if deserializer.input.is_empty() {
        Ok(t)
    } else {
        Err(deserializer.error(ErrorCode::UnexpectedTrailingCharacters))
    }
}

/// Rudano's deserializer. Its recommended that you use `from_str` instead.
pub struct Deserializer<'de> {
    input: &'de str,
    original_input: &'de str,
}

impl<'de> Deserializer<'de> {
    /// Creates a deserializer from the given `str`.
    pub fn from_str(input: &'de str) -> Self {
        Deserializer {
            input,
            original_input: input,
        }
    }
}

impl<'de> Deserializer<'de> {
    fn error(&self, code: ErrorCode) -> Error {
        self.error_at(code, self.input)
    }

    fn error_at(&self, code: ErrorCode, input_slice: &str) -> Error {
        let position = subslice_offset(self.original_input, input_slice);
        let newlines = self
            .original_input
            .chars()
            .take(position)
            .filter(|&c| c == '\n')
            .count();
        let last_newline_position = self.original_input[..position].rfind('\n');

        let line = NonZeroUsize::new(newlines + 1);
        let column = last_newline_position
            .map(|newline| position - newline)
            .unwrap_or(position + 1);
        let column = NonZeroUsize::new(column);

        Error { code, line, column }
    }

    fn get_base(&self, input: &str) -> Result<u32> {
        let mut chars = input.chars();
        match chars
            .next()
            .ok_or_else(|| self.error_at(ErrorCode::UnexpectedEof, input))?
        {
            '0' => match chars.next().unwrap_or('0') {
                'b' => Ok(2),
                'o' => Ok(8),
                'x' => Ok(16),
                _ => Ok(10),
            },
            _ => Ok(10),
        }
    }

    fn count_digits(&self, input: &str, base: u32) -> usize {
        input
            .char_indices()
            .find(|(_i, c)| !(c.is_digit(base) || *c == '_'))
            .map(|(i, _c)| i)
            .unwrap_or(input.len())
    }

    fn peek_char(&self) -> Result<char> {
        self.input
            .chars()
            .next()
            .ok_or_else(|| self.error(ErrorCode::UnexpectedEof))
    }

    fn next_char(&mut self) -> Result<char> {
        let ch = self.peek_char()?;
        self.input = &self.input[ch.len_utf8()..];
        Ok(ch)
    }

    fn skip_whitespace_inner(&mut self) -> Result<()> {
        while self.peek_char()?.is_whitespace() {
            self.next_char()?;
        }
        if self.input.starts_with("//") {
            let (i, _) = self
                .input
                .char_indices()
                .find(|e| e.1 == '\n')
                .unwrap_or((self.input.len(), '\n'));
            // Does not go past the \n, but will try to skip whitespace again anyway
            self.input = &self.input[i..];
            self.skip_whitespace_inner()?;
        }
        Ok(())
    }

    /// Skips all unicode whitespace and comments.
    fn skip_whitespace(&mut self) -> Result<()> {
        match self.skip_whitespace_inner() {
            Err(Error {
                code: ErrorCode::UnexpectedEof,
                ..
            }) => Ok(()),
            r => r,
        }
    }

    /// Parses rudano identifier `true` or `false`.
    fn parse_bool(&mut self) -> Result<bool> {
        if self.input.starts_with("true") {
            self.input = &self.input["true".len()..];
            Ok(true)
        } else if self.input.starts_with("false") {
            self.input = &self.input["false".len()..];
            Ok(false)
        } else {
            Err(self.error(ErrorCode::ExpectedBoolean(self.peek_char()?)))
        }
    }

    fn parse_integer<T: Num + FromPrimitive + Copy + CheckedMul + CheckedAdd + CheckedSub>(
        &mut self,
    ) -> Result<T> {
        let mut new_input = self.input;
        // Gets sign and removes it from input
        let negative = if self.peek_char()? == '-' {
            new_input = &self.input['-'.len_utf8()..];
            true
        } else {
            false
        };

        // Gets base and removes it from input
        let base = self.get_base(new_input)?;
        let n_base = T::from_u32(base).unwrap(); // Cannot fail as maximum returned value is 16
        if base != 10 {
            new_input = &new_input[2..];
        }

        // Gets position where the number (without suffix) itself ends
        let end_i = self.count_digits(new_input, base);
        if end_i == 0 {
            let next_char = new_input
                .chars()
                .next()
                .ok_or_else(|| self.error(ErrorCode::UnexpectedEof))?;
            return Err(self.error(ErrorCode::ExpectedInteger(next_char)));
        }

        // Gets position where the literal (including suffix) ends
        let end_literal_i = new_input
            .char_indices()
            .skip(end_i)
            .find(|(_i, c)| !c.is_alphanumeric())
            .map(|(i, _c)| i)
            .unwrap_or(new_input.len()); // Assume it ends at the end of input

        // As of now, no suffix is allowed
        if end_i != end_literal_i {
            return Err(self.error(ErrorCode::InvalidSuffix(
                new_input[end_i..end_literal_i].to_string(),
            )));
        }

        let mut chars = new_input[..end_i].chars().filter(|c| *c != '_');

        let v = if negative {
            // base * acc - c.to_digit(base)
            chars.try_fold(T::zero(), |acc, c| {
                n_base
                    .checked_mul(&acc)?
                    .checked_sub(&T::from_u32(c.to_digit(base).unwrap()).unwrap())
            })
        } else {
            // base * acc - c.to_digit(base)
            chars.try_fold(T::zero(), |acc, c| {
                n_base
                    .checked_mul(&acc)?
                    .checked_add(&T::from_u32(c.to_digit(base).unwrap()).unwrap())
            })
        }
        .ok_or_else(|| {
            self.error(ErrorCode::IntegerOutOfRange(
                self.input[..end_i + subslice_offset(self.input, new_input)].to_string(),
            ))
        })?;

        self.input = &new_input[end_i..];
        Ok(v)
    }

    fn parse_f32(&mut self) -> Result<f32> {
        match self.peek_char()? {
            '0'..='9' => (),
            '-' if self.input.starts_with("-inf") => {
                self.input = &self.input["-inf".len()..];
                return Ok(std::f32::NEG_INFINITY);
            }
            '-' => (),
            _ => {
                return if self.input.starts_with("inf") {
                    self.input = &self.input["inf".len()..];
                    Ok(std::f32::INFINITY)
                } else if self.input.starts_with("NaN") {
                    self.input = &self.input["NaN".len()..];
                    Ok(std::f32::NAN)
                } else {
                    Err(self.error(ErrorCode::ExpectedFloat(self.peek_char()?)))
                }
            }
        }

        let mut char_indices = self.input.char_indices();
        let end_i = loop {
            if let Some((i, c)) = char_indices.next() {
                match c {
                    '0'..='9' | '-' | '+' | '.' | 'e' | 'E' => (),
                    _ => break i,
                }
            } else {
                break self.input.len();
            }
        };

        let v = self.input[..end_i].parse().map_err(|_| {
            match self
                .peek_char()
                .map(|c| self.error(ErrorCode::ExpectedFloat(c)))
            {
                Ok(err) | Err(err) => err,
            }
        })?;
        self.input = &self.input[end_i..];
        Ok(v)
    }

    fn parse_f64(&mut self) -> Result<f64> {
        match self.peek_char()? {
            '0'..='9' => (),
            '-' if self.input.starts_with("-inf") => {
                self.input = &self.input["-inf".len()..];
                return Ok(std::f64::NEG_INFINITY);
            }
            '-' => (),
            _ => {
                return if self.input.starts_with("inf") {
                    self.input = &self.input["inf".len()..];
                    Ok(std::f64::INFINITY)
                } else if self.input.starts_with("NaN") {
                    self.input = &self.input["NaN".len()..];
                    Ok(std::f64::NAN)
                } else {
                    Err(self.error(ErrorCode::ExpectedFloat(self.peek_char()?)))
                }
            }
        }

        let mut char_indices = self.input.char_indices();
        let end_i = loop {
            if let Some((i, c)) = char_indices.next() {
                match c {
                    '0'..='9' | '-' | '+' | '.' | 'e' | 'E' => (),
                    _ => break i,
                }
            } else {
                break self.input.len();
            }
        };

        let v = self.input[..end_i].parse().map_err(|_| {
            match self
                .peek_char()
                .map(|c| self.error(ErrorCode::ExpectedFloat(c)))
            {
                // Unwraps both ExpectedFloat error and peek_char's error
                Ok(err) | Err(err) => err,
            }
        })?;
        self.input = &self.input[end_i..];
        Ok(v)
    }

    fn parse_char_inner(&mut self, delimiter: char) -> Result<ParseCharResult> {
        use ParseCharResult::*;

        Ok(match self.next_char()? {
            c if c == delimiter => return Ok(Delimiter),
            '\\' => match self.next_char()? {
                // Quote escapes
                '\'' => EscapedChar('\''),
                '"' => EscapedChar('"'),
                // ASCII escapes
                'x' => {
                    let c = u32::from_str_radix(
                        &self
                            .input
                            .get(..2)
                            .ok_or_else(|| self.error(ErrorCode::UnexpectedEof))?,
                        16,
                    )
                    .ok()
                    .and_then(std::char::from_u32)
                    .ok_or_else(|| {
                        self.error(ErrorCode::HexEscapeInvalid(self.input[..2].to_string()))
                    })?;
                    self.input = &self.input[2..];
                    EscapedChar(c)
                }
                'n' => EscapedChar('\n'),
                'r' => EscapedChar('\r'),
                't' => EscapedChar('\t'),
                '\\' => EscapedChar('\\'),
                '0' => EscapedChar('\0'),
                // Unicode escapes
                'u' => {
                    match self.next_char()? {
                        '{' => (),
                        c => return Err(self.error(ErrorCode::ExpectedEscapeStart(c))),
                    }

                    let end_i = self
                        .input
                        .find('}')
                        .ok_or_else(|| self.error(ErrorCode::ExpectedEscapeStop))?;

                    let c = u32::from_str_radix(&self.input[..end_i], 16)
                        .ok()
                        .and_then(std::char::from_u32)
                        .ok_or_else(|| {
                            self.error(ErrorCode::UnicodeEscapeInvalid(
                                self.input[..end_i].to_string(),
                            ))
                        })?;

                    self.input = &self.input[end_i + '}'.len_utf8()..];
                    EscapedChar(c)
                }
                c => return Err(self.error(ErrorCode::EscapeInvalid(c))),
            },
            c => NormalChar(c),
        })
    }

    fn parse_char(&mut self) -> Result<char> {
        match self.next_char()? {
            '\'' => (),
            c => return Err(self.error(ErrorCode::ExpectedCharStart(c))),
        }

        let c = self
            .parse_char_inner('\'')?
            .char()
            .ok_or_else(|| self.error(ErrorCode::ExpectedChar))?;

        match self.next_char()? {
            '\'' => (),
            c => return Err(self.error(ErrorCode::ExpectedCharEnd(c))),
        }
        Ok(c)
    }

    fn parse_string(&mut self) -> Result<Cow<'de, str>> {
        use ParseCharResult::*;

        match self.next_char()? {
            '"' => (),
            c => return Err(self.error(ErrorCode::ExpectedStringStart(c))),
        }

        // Saves original input as self.next_char modifies it
        let input = self.input;

        // Number of characters that are exactly the same as in the input
        let mut unescaped_count = 0;
        let cow = loop {
            match self.parse_char_inner('"')? {
                Delimiter => break Cow::from(&input[..unescaped_count]),
                NormalChar(c) => unescaped_count += c.len_utf8(),
                EscapedChar(c) => {
                    // If any char has been escaped, create a new String to hold the data and
                    // deserializes the the remaining data into it.
                    let mut string = String::from(&input[..unescaped_count]);
                    string.push(c);
                    while let Some(c) = self.parse_char_inner('"')?.char() {
                        string.push(c);
                    }
                    break Cow::from(string);
                }
            }
        };
        Ok(cow)
    }

    #[allow(clippy::collapsible_if)]
    fn parse_identifier(&mut self) -> Result<&'de str> {
        let mut count = 0;
        let mut char_indices = self.input.char_indices();
        let end_i = loop {
            if let Some((i, c)) = char_indices.next() {
                if !c.is_ascii_alphanumeric() && c != '_' {
                    break i;
                }
                count += 1;
            } else {
                break self.input.len();
            }
        };

        // Checks if first char was a digit
        if self.peek_char()?.is_digit(10) {
            return Err(self.error(ErrorCode::ExpectedIdentifier(self.peek_char()?)));
        }

        // Checks if first char was an _
        if self.peek_char()? == '_' {
            if count <= 1 {
                return Err(self.error(ErrorCode::ExpectedIdentifier(self.peek_char()?)));
            }
        } else {
            if count < 1 {
                return Err(self.error(ErrorCode::ExpectedIdentifier(self.peek_char()?)));
            }
        }

        let v = &self.input[..end_i];
        self.input = &self.input[end_i..];
        Ok(v)
    }

    fn parse_any_integer(&mut self) -> Result<Integer> {
        let mut new_input = self.input;
        // Gets sign and removes it from input
        let negative = if self.peek_char()? == '-' {
            new_input = &self.input['-'.len_utf8()..];
            true
        } else {
            false
        };

        // Gets base and removes it from input
        let base = self.get_base(new_input)?;
        if base != 10 {
            new_input = &new_input[2..];
        }

        // Gets position where the number (without suffix) itself ends
        let end_i = self.count_digits(new_input, base);
        if end_i == 0 {
            let next_char = new_input
                .chars()
                .next()
                .ok_or_else(|| self.error(ErrorCode::UnexpectedEof))?;
            return Err(self.error(ErrorCode::ExpectedInteger(next_char)));
        }

        // Ending integers with '.' is disallowed, as it would be a float.
        if let Some(next_char @ '.') = new_input.chars().nth(end_i) {
            return Err(self.error(ErrorCode::ExpectedInteger(next_char)));
        }

        // Gets position where the literal (including suffix) ends
        let end_literal_i = new_input
            .char_indices()
            .skip(end_i)
            .find(|(_i, c)| !c.is_alphanumeric())
            .map(|(i, _c)| i)
            .unwrap_or(new_input.len()); // Assume it ends at the end of input

        // As of now, no suffix is allowed
        if end_i != end_literal_i {
            return Err(self.error(ErrorCode::InvalidSuffix(
                new_input[end_i..end_literal_i].to_string(),
            )));
        }

        let mut chars = new_input[..end_i].chars().filter(|c| *c != '_');

        let integer = if negative {
            let n_base = i128::from_u32(base).unwrap(); // Cannot fail as maximum returned value is 16
                                                        // base * acc - c.to_digit(base)
            let n = chars
                .try_fold(i128::zero(), |acc, c| {
                    n_base
                        .checked_mul(acc)?
                        .checked_sub(i128::from_u32(c.to_digit(base).unwrap()).unwrap())
                })
                .ok_or_else(|| {
                    self.error(ErrorCode::IntegerOutOfRange(
                        self.input[..end_i + subslice_offset(self.input, new_input)].to_string(),
                    ))
                })?;

            // We should just use i128::from when const from is stabilized:
            // Tracking issue: https://github.com/rust-lang/rust/issues/87852
            const MIN_I8: i128 = i8::MIN as i128;
            const MIN_I16: i128 = i16::MIN as i128;
            const MIN_I32: i128 = i32::MIN as i128;
            const MIN_I64: i128 = i64::MIN as i128;
            const MIN_I128: i128 = i128::MIN;

            match n {
                MIN_I8.. => Integer::I8(n as i8),
                MIN_I16.. => Integer::I16(n as i16),
                MIN_I32.. => Integer::I32(n as i32),
                MIN_I64.. => Integer::I64(n as i64),
                MIN_I128.. => Integer::I128(n as i128),
                // _ => panic!(),
            }
        } else {
            let n_base = u128::from_u32(base).unwrap(); // Cannot fail as maximum returned value is 16
                                                        // base * acc - c.to_digit(base)
            let n = chars
                .try_fold(u128::zero(), |acc, c| {
                    n_base
                        .checked_mul(acc)?
                        .checked_add(u128::from_u32(c.to_digit(base).unwrap()).unwrap())
                })
                .ok_or_else(|| {
                    self.error(ErrorCode::IntegerOutOfRange(
                        self.input[..end_i + subslice_offset(self.input, new_input)].to_string(),
                    ))
                })?;

            // We should just use i128::from when const from is stabilized:
            // Tracking issue: https://github.com/rust-lang/rust/issues/87852
            const MAX_U8: u128 = u8::MAX as u128;
            const MAX_U16: u128 = u16::MAX as u128;
            const MAX_U32: u128 = u32::MAX as u128;
            const MAX_U64: u128 = u64::MAX as u128;
            const MAX_U128: u128 = u128::MAX;

            match n {
                0..=MAX_U8 => Integer::U8(n as u8),
                0..=MAX_U16 => Integer::U16(n as u16),
                0..=MAX_U32 => Integer::U32(n as u32),
                0..=MAX_U64 => Integer::U64(n as u64),
                0..=MAX_U128 => Integer::U128(n as u128),
            }
        };

        self.input = &new_input[end_i..];
        Ok(integer)
    }

    fn deserialize_struct_inner<V: Visitor<'de>>(
        &mut self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value> {
        // Parses opening brace of the struct.
        if self.next_char()? == '{' {
            // Give the visitor access to each entry of the struct.
            let value = visitor.visit_map(CommaSeparatedStruct::new(self))?;
            // Parses the closing brace of the struct.
            self.skip_whitespace()?;
            match self.next_char()? {
                '}' => Ok(value),
                c => Err(self.error(ErrorCode::ExpectedStructEnd(c))),
            }
        } else {
            Err(self.error(ErrorCode::ExpectedStructOpening(self.peek_char()?)))
        }
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.skip_whitespace()?;
        let original_input = self.input;

        if let Ok(b) = self.parse_bool() {
            visitor.visit_bool(b)
        } else if let Ok(n) = self.parse_any_integer() {
            use Integer::*;
            match n {
                I8(n) => visitor.visit_i8(n),
                I16(n) => visitor.visit_i16(n),
                I32(n) => visitor.visit_i32(n),
                I64(n) => visitor.visit_i64(n),
                I128(n) => visitor.visit_i128(n),
                U8(n) => visitor.visit_u8(n),
                U16(n) => visitor.visit_u16(n),
                U32(n) => visitor.visit_u32(n),
                U64(n) => visitor.visit_u64(n),
                U128(n) => visitor.visit_u128(n),
            }
        } else if let Ok(n) = self.parse_f64() {
            visitor.visit_f64(n)
        } else if self.peek_char()? == '\'' {
            self.deserialize_char(visitor)
        } else if self.peek_char()? == '"' {
            self.deserialize_str(visitor)
        } else if self.peek_char()? == '[' {
            // Checks if it can deserialize as a sequence
            if self.deserialize_seq(IgnoredAny).is_ok() {
                self.input = original_input;
                self.deserialize_seq(visitor)
            } else {
                self.input = original_input;
                self.deserialize_map(visitor)
            }
        } else if self.peek_char()? == '(' {
            if self.deserialize_unit(IgnoredAny).is_ok() {
                visitor.visit_unit()
            } else {
                self.input = original_input;
                self.deserialize_tuple(0, visitor)
            }
        } else if let Ok(id) = self.parse_identifier() {
            match id {
                "Some" | "None" => {
                    self.input = original_input;
                    self.deserialize_option(visitor)
                }
                _id => {
                    self.skip_whitespace()?;
                    // peek_char's error can only be EOF, in which case it should also map to the unit type
                    match self.peek_char().unwrap_or('_') {
                        '{' => {
                            // If it's empty, serde understands it as a "Unit Struct" and we need to use visit_unit
                            let mut temp_deserializer = Deserializer {
                                input: self.input,
                                original_input: self.original_input,
                            };
                            temp_deserializer.next_char()?;
                            temp_deserializer.skip_whitespace()?;
                            match temp_deserializer.next_char()? {
                                '}' => {
                                    self.input = temp_deserializer.input;
                                    visitor.visit_unit()
                                }
                                _ => self.deserialize_struct_inner(&[], visitor),
                            }
                        }
                        '(' => self.deserialize_tuple(0, visitor),
                        _ => visitor.visit_unit(),
                    }
                }
            }
        } else {
            Err(self.error(ErrorCode::ExpectedAny))
        }
    }

    fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_bool(self.parse_bool()?)
    }

    fn deserialize_i8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_i8(self.parse_integer()?)
    }

    fn deserialize_i16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_i16(self.parse_integer()?)
    }

    fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_i32(self.parse_integer()?)
    }

    fn deserialize_i64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_i64(self.parse_integer()?)
    }

    fn deserialize_i128<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_i128(self.parse_integer()?)
    }

    fn deserialize_u8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_u8(self.parse_integer()?)
    }

    fn deserialize_u16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_u16(self.parse_integer()?)
    }

    fn deserialize_u32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_u32(self.parse_integer()?)
    }

    fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_u64(self.parse_integer()?)
    }

    fn deserialize_u128<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_u128(self.parse_integer()?)
    }

    fn deserialize_f32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_f32(self.parse_f32()?)
    }

    fn deserialize_f64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_f64(self.parse_f64()?)
    }

    fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        visitor.visit_char(self.parse_char()?)
    }

    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.skip_whitespace()?;
        match self.parse_string()? {
            Cow::Borrowed(b) => visitor.visit_borrowed_str(b),
            Cow::Owned(string) => visitor.visit_string(string),
        }
    }

    fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_byte_buf<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        // Checks if None
        self.skip_whitespace()?;
        if self.input.starts_with("None") {
            self.input = &self.input["None".len()..];
            visitor.visit_none()

        // Checks if Some
        } else if self.input.starts_with("Some") {
            self.input = &self.input["Some".len()..];

            self.skip_whitespace()?;
            match self.next_char()? {
                '(' => (),
                c => return Err(self.error(ErrorCode::ExpectedTupleStart(c))),
            }

            let v = visitor.visit_some(&mut *self)?;

            self.skip_whitespace()?;
            match self.next_char()? {
                ')' => (),
                c => return Err(self.error(ErrorCode::ExpectedTupleEnd(c))),
            }
            Ok(v)
        } else {
            Err(self.error(ErrorCode::ExpectedOption(self.peek_char()?)))
        }
    }

    // Units are values containing no data, in Rudano represented by the Rust's Unit type "()".
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.skip_whitespace()?;
        match self.next_char()? {
            '(' => {
                self.skip_whitespace()?;
                match self.next_char()? {
                    ')' => visitor.visit_unit(),
                    c => Err(self.error(ErrorCode::ExpectedUnitEnd(c))),
                }
            }
            c => Err(self.error(ErrorCode::ExpectedUnitStart(c))),
        }
    }

    /// Unit struct are named values containing no data.
    fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.skip_whitespace()?;
        let identifier = self.parse_identifier()?;
        if identifier != name {
            return Err(self.error(ErrorCode::WrongStructName(
                name.to_string(),
                identifier.to_string(),
            )));
        }

        self.skip_whitespace()?;
        // Checks for optional {}, ignoring EOF
        let has_brace = self.peek_char().or_else(|e| {
            if e.code == ErrorCode::UnexpectedEof {
                Ok('\n')
            } else {
                Err(e)
            }
        })? == '{';
        if has_brace {
            self.next_char()?;
            self.skip_whitespace()?;
            match self.peek_char()? {
                '}' => self.next_char()?,
                c => return Err(self.error(ErrorCode::ExpectedStructEnd(c))),
            };
        }

        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.skip_whitespace()?;
        let identifier = self.parse_identifier()?;
        if identifier != name {
            return Err(self.error(ErrorCode::WrongStructName(
                name.to_string(),
                identifier.to_string(),
            )));
        }

        // Parses opening parenthesis (or brackets, needed for arrays) of the tuple.
        self.skip_whitespace()?;
        match self.next_char()? {
            '(' => {
                self.skip_whitespace()?;
                let value = visitor.visit_newtype_struct(&mut *self)?;
                self.skip_whitespace()?;
                match self.next_char()? {
                    ')' => Ok(value),
                    c => Err(self.error(ErrorCode::ExpectedTupleEnd(c))),
                }
            }
            c => Err(self.error(ErrorCode::ExpectedTupleStart(c))),
        }
    }

    fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Parse the opening bracket of the sequence.
        self.skip_whitespace()?;
        match self.next_char()? {
            '[' => {
                // Give the visitor access to each element of the sequence.
                let value = visitor.visit_seq(CommaSeparated::new(&mut self))?;
                // Parse the closing bracket of the sequence.
                self.skip_whitespace()?;
                match self.next_char()? {
                    ']' => Ok(value),
                    c => Err(self.error(ErrorCode::ExpectedArrayEnd(c))),
                }
            }
            c => Err(self.error(ErrorCode::ExpectedArrayStart(c))),
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Parses opening parenthesis (or brackets, needed for arrays) of the tuple.
        self.skip_whitespace()?;
        let parenthesis = match self.next_char()? {
            '(' => true,
            '[' => false,
            c => return Err(self.error(ErrorCode::ExpectedTupleStart(c))),
        };

        let value = visitor.visit_seq(CommaSeparated::new(&mut *self))?;

        // Skips trailing comma if needed
        self.skip_whitespace()?;
        if self.peek_char()? == ',' {
            self.next_char()?;
            self.skip_whitespace()?;
        }

        // Parses the closing parenthesis/bracket of the tuple.
        let ending_char = self.next_char()?;
        if ending_char == if parenthesis { ')' } else { ']' } {
            Ok(value)
        } else {
            Err(self.error(ErrorCode::ExpectedTupleEnd(ending_char)))
        }
    }

    // Tuple structs, in Rudano, are tuples with a name.
    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.skip_whitespace()?;
        let identifier = self.parse_identifier()?;
        if identifier != name {
            return Err(self.error(ErrorCode::WrongStructName(
                name.to_string(),
                identifier.to_string(),
            )));
        }

        self.skip_whitespace()?;
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Parse the opening brace of the map.
        self.skip_whitespace()?;
        match self.next_char()? {
            '[' => {
                // Gives the visitor access to each entry of the map.
                let value = visitor.visit_map(CommaSeparated::new(&mut self))?;

                // Skips trailing comma if needed
                self.skip_whitespace()?;
                if self.peek_char()? == ',' {
                    self.next_char()?;
                    self.skip_whitespace()?;
                }

                // Parses the closing brace of the map.
                self.skip_whitespace()?;
                match self.next_char()? {
                    ']' => Ok(value),
                    c => Err(self.error(ErrorCode::ExpectedMapEnd(c))),
                }
            }
            c => Err(self.error(ErrorCode::ExpectedMapStart(c))),
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.skip_whitespace()?;
        let identifier = self.parse_identifier()?;
        if identifier != name {
            return Err(self.error(ErrorCode::WrongStructName(
                name.to_string(),
                identifier.to_string(),
            )));
        }

        self.skip_whitespace()?;
        self.deserialize_struct_inner(fields, visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(Enum::new(self))
    }

    /// Deserializes identifiers, used to identify struct fields and enum variants
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.skip_whitespace()?;
        let id = self.parse_identifier()?;
        visitor.visit_str(id)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

struct CommaSeparated<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    /// Whether we are in the first element of the sequence/map/array
    first: bool,
}

impl<'a, 'de> CommaSeparated<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        CommaSeparated { de, first: true }
    }
}

impl<'de, 'a> SeqAccess<'de> for CommaSeparated<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        // Checks if there is comma (before entry OR trailing)
        self.de.skip_whitespace()?;
        let has_comma = if self.de.peek_char()? == ',' {
            self.de.next_char()?;
            true
        } else {
            false
        };

        // Checks if there are no more elements.
        self.de.skip_whitespace()?;
        if self.de.peek_char()? == ']' || self.de.peek_char()? == ')' {
            return Ok(None);
        }

        // Comma is required before every element except the first.
        if !self.first && !has_comma {
            return Err(self
                .de
                .error(ErrorCode::ExpectedArrayComma(self.de.peek_char()?)));
        }

        self.first = false;

        // Deserializes array element.
        seed.deserialize(&mut *self.de).map(Some)
    }
}

impl<'de, 'a> MapAccess<'de> for CommaSeparated<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        // Checks if there is comma before entry / trailing comma
        self.de.skip_whitespace()?;
        let has_comma = if self.de.peek_char()? == ',' {
            self.de.next_char()?;
            true
        } else {
            false
        };

        // Checks if there are no more entries.
        self.de.skip_whitespace()?;
        if self.de.peek_char()? == ']' || self.de.peek_char()? == '}' {
            return Ok(None);
        }

        // Comma is required before every entry except the first.
        if !self.first && !has_comma {
            return Err(self
                .de
                .error(ErrorCode::ExpectedMapStructComma(self.de.peek_char()?)));
        }

        self.first = false;

        // Deserializes map key.
        self.de.skip_whitespace()?;
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        match self.de.next_char()? {
            ':' => (),
            c => return Err(self.de.error(ErrorCode::ExpectedMapStructColon(c))),
        }

        // Deserializes map value.
        seed.deserialize(&mut *self.de)
    }
}

// Needed specifically for deserialize_any
struct CommaSeparatedStruct<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    /// Whether we are in the first element of the struct
    first: bool,
}

impl<'a, 'de> CommaSeparatedStruct<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        CommaSeparatedStruct { de, first: true }
    }
}

impl<'de, 'a> MapAccess<'de> for CommaSeparatedStruct<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        // Checks if there is comma before entry / trailing comma
        self.de.skip_whitespace()?;
        let has_comma = if self.de.peek_char()? == ',' {
            self.de.next_char()?;
            true
        } else {
            false
        };

        // Checks if there are no more entries.
        self.de.skip_whitespace()?;
        if self.de.peek_char()? == ']' || self.de.peek_char()? == '}' {
            return Ok(None);
        }

        // Comma is required before every entry except the first.
        if !self.first && !has_comma {
            return Err(self
                .de
                .error(ErrorCode::ExpectedMapStructComma(self.de.peek_char()?)));
        }

        self.first = false;

        // Deserializes map key.
        self.de.skip_whitespace()?;

        seed.deserialize(StructIdentifierDeserializer(&mut *self.de))
            .map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        match self.de.next_char()? {
            ':' => (),
            c => return Err(self.de.error(ErrorCode::ExpectedMapStructColon(c))),
        }

        // Deserializes map value.
        seed.deserialize(&mut *self.de)
    }
}

struct StructIdentifierDeserializer<'a, 'de>(&'a mut Deserializer<'de>);

impl<'a, 'de> de::Deserializer<'de> for StructIdentifierDeserializer<'a, 'de> {
    type Error = <&'a mut Deserializer<'de> as de::Deserializer<'de>>::Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_identifier(visitor)
    }

    fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_bool(visitor)
    }

    fn deserialize_i8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_i8(visitor)
    }

    fn deserialize_i16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_i16(visitor)
    }

    fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_i32(visitor)
    }

    fn deserialize_i64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_i64(visitor)
    }

    fn deserialize_i128<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_i128(visitor)
    }

    fn deserialize_u8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_u8(visitor)
    }

    fn deserialize_u16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_u16(visitor)
    }

    fn deserialize_u32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_u32(visitor)
    }

    fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_u64(visitor)
    }

    fn deserialize_u128<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_u128(visitor)
    }

    fn deserialize_f32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_f32(visitor)
    }

    fn deserialize_f64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_f64(visitor)
    }

    fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_char(visitor)
    }

    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_str(visitor)
    }

    fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_string(visitor)
    }

    fn deserialize_bytes<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_bytes(visitor)
    }

    fn deserialize_byte_buf<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_byte_buf(visitor)
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        self.0.deserialize_option(visitor)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_unit(visitor)
    }

    fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_unit_struct(name, visitor)
    }

    fn deserialize_newtype_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_newtype_struct(name, visitor)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_seq(visitor)
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_tuple(len, visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_tuple_struct(name, len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_map(visitor)
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_struct(name, fields, visitor)
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_enum(name, variants, visitor)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_identifier(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_ignored_any(visitor)
    }
}

struct Enum<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> Enum<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Enum { de }
    }
}

impl<'de, 'a> EnumAccess<'de> for Enum<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: DeserializeSeed<'de>,
    {
        // Deserializes the variant seed (identifier)
        self.de.skip_whitespace()?;
        let val = seed.deserialize(&mut *self.de)?;
        Ok((val, self))
    }
}

impl<'de, 'a> VariantAccess<'de> for Enum<'a, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        self.de.skip_whitespace()?;
        // Checks for optional {}, ignoring EOF
        let has_brace = self.de.peek_char().or_else(|e| {
            if e.code == ErrorCode::UnexpectedEof {
                Ok('\n')
            } else {
                Err(e)
            }
        })? == '{';
        if has_brace {
            self.de.next_char()?;
            self.de.skip_whitespace()?;
            match self.de.peek_char()? {
                '}' => self.de.next_char()?,
                c => return Err(self.de.error(ErrorCode::ExpectedStructEnd(c))),
            };
        }

        Ok(())
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value> {
        self.de.skip_whitespace()?;
        match self.de.next_char()? {
            '(' => (),
            c => return Err(self.de.error(ErrorCode::ExpectedTupleStart(c))),
        }

        let v = seed.deserialize(&mut *self.de)?;

        self.de.skip_whitespace()?;
        match self.de.next_char()? {
            ')' => (),
            c => return Err(self.de.error(ErrorCode::ExpectedTupleEnd(c))),
        }
        Ok(v)
    }

    fn tuple_variant<V: Visitor<'de>>(self, len: usize, visitor: V) -> Result<V::Value> {
        de::Deserializer::deserialize_tuple(self.de, len, visitor)
    }

    fn struct_variant<V>(self, fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.de.skip_whitespace()?;
        self.de.deserialize_struct_inner(fields, visitor)
    }
}

#[cfg(test)]
mod tests {
    use super::from_str;
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;
    use {
        crate::error::DeserializationErrorCode as ErrorCode, crate::DeserializationError as Error,
    };

    #[test]
    fn borrowed_str() {
        let serialized = r#""my string""#;
        let borrowed: &str =
            from_str(serialized).expect(&format!("Could not deserialize {}", serialized));
        assert_eq!("my string", borrowed);
    }

    #[test]
    fn comment() {
        let array = &[12, 30, 213];
        let serialized = r"[
            12,
            30, // There is a comment here!
            213,

            // Another comment here!!!
        ]";

        assert_eq!(
            array,
            &from_str::<Vec<u8>>(&serialized)
                .expect(&format!("Could not deserialize {}", serialized))[..]
        );
    }

    #[test]
    fn struct_with_unused_fields() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct MyStruct<'a> {
            a: u32,
            b: char,
            d: u16,
            e: &'a str,
        }

        let expected = MyStruct {
            a: 1000,
            b: 'c',
            d: 29,
            e: "yo",
        };

        let text = r#"MyStruct {
    a: 1000,
    b: 'c',
    c: 10,
    d: 29,
    e: "yo",
    f: "lol",
}"#;
        assert_eq!(from_str::<MyStruct>(text).unwrap(), expected);
    }

    #[test]
    fn u16_out_of_range() {
        let values = [
            "200000",
            "-2",
            "-001000",
            "000100000",
            "0x10000",
            "0b1000000000000000000000",
        ];
        for text in values.iter() {
            let deserialized: Result<u16, Error> = from_str(text);
            assert!(match deserialized {
                Err(Error {
                    code: ErrorCode::IntegerOutOfRange(_),
                    ..
                }) => true,
                _ => false,
            });
        }
    }

    #[test]
    fn optional_struct_identifier_fail() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Test {
            a: u8,
        }

        let text = r#"{
    a: 10,
}"#;
        assert!(from_str::<Test>(text)
            .err()
            .filter(|e| e.code == ErrorCode::ExpectedIdentifier('{'))
            .is_some());
    }

    #[test]
    fn refuse_curly_for_maps() {
        let text = r#"{
    "a": 29,
    "b": 100,
}"#;
        let deserialized = from_str::<HashMap<&str, u8>>(&text);
        assert_eq!(
            deserialized.unwrap_err().code,
            ErrorCode::ExpectedMapStart('{')
        );
    }

    #[test]
    fn refuse_str_as_identifier() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Test {
            a: u8,
            b: u16,
        }

        let text = r#"{
            a: 10,
            "b": 27,
        }"#;
        match from_str::<Test>(&text) {
            Ok(_) => panic!("should fail parsing"),
            Err(Error {
                code: ErrorCode::ExpectedIdentifier(_),
                ..
            }) => (),
            _ => panic!("wrong error"),
        }
    }

    #[test]
    fn boolean() {
        let values = [(true, r"true"), (false, r"false")];
        for (expected, text) in values.iter() {
            let deserialized: bool =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn uint8() {
        let random: u8 = rand::random();
        let string = random.to_string();
        let values = [
            (27u8, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (0b11010, r"0b11010"),
            (std::u8::MIN, r"0"),
            (std::u8::MAX, r"255"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: u8 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn uint8_invalid() {
        let text = r"2f";
        let deserialized: Result<u8, _> = from_str(text);
        match deserialized {
            Err(Error {
                code: ErrorCode::InvalidSuffix(suffix),
                ..
            }) if suffix == "f" => (),
            _ => panic!("{:?}", deserialized),
        }
    }

    #[test]
    fn uint16() {
        let random: u16 = rand::random();
        let string = random.to_string();
        let values = [
            (27u16, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (0b11010, r"0b11010"),
            (std::u16::MIN, r"0"),
            (std::u16::MAX, r"65535"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: u16 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn uint32() {
        let random: u32 = rand::random();
        let string = random.to_string();
        let values = [
            (27u32, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (0b11010, r"0b11010"),
            (std::u32::MIN, r"0"),
            (std::u32::MAX, r"4294967295"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: u32 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn uint64() {
        let random: u64 = rand::random();
        let string = random.to_string();
        let values = [
            (27u64, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (0b11010, r"0b11010"),
            (std::u64::MIN, r"0"),
            (std::u64::MAX, r"18446744073709551615"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: u64 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn uint128() {
        let random: u128 = rand::random();
        let string = random.to_string();
        let values = [
            (27u128, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (0b11010, r"0b11010"),
            (std::u128::MIN, r"0"),
            (std::u128::MAX, r"340282366920938463463374607431768211455"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: u128 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn int8() {
        let random: i8 = rand::random();
        let string = random.to_string();
        let values = [
            (27i8, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (-0x1F, r"-0x1F"),
            (0b11010, r"0b11010"),
            (-37, r"-37"),
            (std::i8::MIN, r"-128"),
            (std::i8::MAX, r"127"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: i8 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn int16() {
        let random: i16 = rand::random();
        let string = random.to_string();
        let values = [
            (27i16, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (-0x1F, r"-0x1F"),
            (0b11010, r"0b11010"),
            (-37, r"-37"),
            (std::i16::MIN, r"-32768"),
            (std::i16::MAX, r"32767"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: i16 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn int32() {
        let random: i32 = rand::random();
        let string = random.to_string();
        let values = [
            (27i32, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (-0x1F, r"-0x1F"),
            (0b11010, r"0b11010"),
            (-37, r"-37"),
            (std::i32::MIN, r"-2147483648"),
            (std::i32::MAX, r"2147483647"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: i32 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn int64() {
        let random: i64 = rand::random();
        let string = random.to_string();
        let values = [
            (27i64, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (-0x1F, r"-0x1F"),
            (0b11010, r"0b11010"),
            (-37, r"-37"),
            (std::i64::MIN, r"-9223372036854775808"),
            (std::i64::MAX, r"9223372036854775807"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: i64 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn int128() {
        let random: i128 = rand::random();
        let string = random.to_string();
        let values = [
            (27i128, r"27"),
            (43, r"43"),
            (0x2A, r"0x2A"),
            (0o72, r"0o72"),
            (-0x1F, r"-0x1F"),
            (0b11010, r"0b11010"),
            (-37, r"-37"),
            (std::i128::MIN, r"-170141183460469231731687303715884105728"),
            (std::i128::MAX, r"170141183460469231731687303715884105727"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: i128 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    pub fn float32() {
        let random: f32 = rand::random();
        let string = random.to_string();
        let values = [
            (27f32, r"27"),
            (43.0, r"43.0"),
            (239.34, r"239.34"),
            (-37.3, r"-37.3"),
            (-3.7e13, r"-3.7e13"),
            (-92., r"-92."),
            (std::f32::INFINITY, r"inf"),
            (std::f32::NEG_INFINITY, r"-inf"),
            (random, &string),
        ];

        for (expected, text) in values.iter() {
            let deserialized: f32 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }

        // NaN != NaN, so this test is special
        assert!(from_str::<f32>("NaN").unwrap().is_nan());
    }

    #[test]
    pub fn float64() {
        let random: f64 = rand::random();
        let string = random.to_string();
        let values = [
            (27f64, r"27"),
            (43.0, r"43.0"),
            (239.34, r"239.34"),
            (-37.3, r"-37.3"),
            (-3.7e13, r"-37000000000000.0"),
            (-92., r"-92."),
            (std::f64::INFINITY, r"inf"),
            (std::f64::NEG_INFINITY, r"-inf"),
            (random, &string),
        ];
        for (expected, text) in values.iter() {
            let deserialized: f64 =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }

        // NaN != NaN, so this test is special
        assert!(from_str::<f64>("NaN").unwrap().is_nan());
    }

    #[test]
    fn character() {
        let values = [
            ('a', r"'a'"),
            ('@', r"'@'"),
            ('\x5A', r"'\x5A'"),
            ('\u{bf0}', r"'\u{bf0}'"),
            ('\u{1f638}', r"'\u{1f638}'"),
            ('😸', r"'😸'"),
            ('\n', r"'\n'"),
            ('\t', r"'\t'"),
            (std::char::MAX, r"'\u{10ffff}'"),
            (std::char::REPLACEMENT_CHARACTER, r"'\u{fffd}'"),
        ];
        for (expected, text) in values.iter() {
            let deserialized: char =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn string() {
        let values = [
            ("a", r#""a""#),
            ("@", r#""@""#),
            (
                "The quick brown fox jumps over the lazy dog",
                r#""The quick brown fox jumps over the lazy dog""#,
            ),
            ("\x5A", r#""\x5A""#),
            ("\u{bf0}\u{1f638}", r#""\u{bf0}\u{1f638}""#),
            ("😸", r#""😸""#),
            ("such\nwow", r#""such\nwow""#),
            ("very\tlol", r#""very\tlol""#),
            ("\u{10ffff}", r#""\u{10ffff}""#),
            ("\u{fffd}", r#""\u{fffd}""#),
        ];
        for (expected, text) in values.iter() {
            let deserialized: String =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn byte_array() {
        let values: [(&[u8], &str); 2] = [
            (
                &[],
                r"[
]",
            ),
            (
                &[12, 49, 129, 20, 30, 213],
                r"[
    12,
    49,
    129,
    20,
    30,
    213,
]",
            ),
        ];
        for (expected, text) in values.iter() {
            let deserialized: Vec<u8> =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    pub fn option() {
        assert_eq!(from_str::<Option<()>>("None").unwrap(), None);
        assert_eq!(from_str::<Option<()>>("Some(())").unwrap(), Some(()));
        assert_eq!(from_str::<Option<u8>>("Some(30)").unwrap(), Some(30));
    }

    #[test]
    pub fn unit() {
        #[derive(PartialEq, Serialize, Deserialize, Debug)]
        struct Unit;

        assert_eq!(from_str::<()>("()").unwrap(), ());
        assert_eq!(from_str::<()>("( )").unwrap(), ());
    }

    #[test]
    pub fn unit_struct() {
        #[derive(PartialEq, Serialize, Deserialize, Debug)]
        struct Unit;

        assert_eq!(from_str::<Unit>("Unit").unwrap(), Unit);
        assert_eq!(from_str::<Unit>("Unit{}").unwrap(), Unit);
        assert_eq!(from_str::<Unit>("Unit {}").unwrap(), Unit);
    }

    #[test]
    pub fn newtype_struct() {
        #[derive(PartialEq, Serialize, Deserialize, Debug)]
        struct Newtype(u8);

        assert_eq!(from_str::<Newtype>("Newtype(39)").unwrap(), Newtype(39));
        assert_eq!(
            from_str::<Newtype>("Newtype ( 0xa8 )").unwrap(),
            Newtype(0xa8)
        );
        assert!(from_str::<Newtype>("0xC8")
            .err()
            .filter(|e| e.code == ErrorCode::ExpectedIdentifier('0'))
            .is_some());
    }

    #[test]
    pub fn enum_variants_in_struct() {
        use Enum::*;

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        enum Enum {
            Unit,
            Newtype(u32),
            Tuple(u32, u32),
            Struct {
                a: u32,
                #[serde(serialize_with = "crate::array_seq::serialize_array")]
                b: [u8; 3],
            },
        }

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Variants {
            unit: Enum,
            unit_2: Enum,
            newtype: Enum,
            tuple: Enum,
            struct_variant: Enum,
        }

        let expected = Variants {
            unit: Unit,
            unit_2: Unit {},
            newtype: Newtype(70),
            tuple: Tuple(20, 80),
            struct_variant: Struct {
                a: 10,
                b: [2, 4, 8],
            },
        };

        let text = r#"Variants {
    unit: Unit,
    unit_2: Unit {},
    newtype: Newtype(70),
    tuple: Tuple(20, 80),
    struct_variant: Struct {
        a: 10,
        b: [
            2,
            4,
            8,
        ],
    },
}"#;
        assert_eq!(from_str::<Variants>(text).unwrap(), expected);
    }

    #[test]
    fn seq() {
        let (expected, text) = (
            vec![],
            r"[
]",
        );
        let deserialized: Vec<u8> = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);

        let (expected, text) = (
            vec![12, 49, 129, 20, 30, 213],
            r"[
    12,
    49,
    129,
    20,
    30,
    213,
]",
        );
        let deserialized: Vec<u8> = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);

        let (expected, text) = (
            vec!['o', '\n', '1', '\t', '😸', '\u{1f638}'],
            r"[
    'o',
    '\n',
    '1',
    '\t',
    '😸',
    '\u{1f638}',
]",
        );
        let deserialized: Vec<char> = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);

        let (expected, text) = (vec![vec![20.3, 19.2, -100.22]], r"[[20.3, 19.2, -100.22,]]");
        let deserialized: Vec<Vec<f32>> = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);
    }

    #[test]
    pub fn tuple() {
        // Actually unit type, which is an empty tuple
        let (expected, text) = ((), r"()");
        let deserialized: () = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);

        let (expected, text) = ((12, 49, 129, 20, 30, 213), r"(12, 49, 129, 20, 30, 213)");
        let deserialized: (u8, u8, u8, u8, u8, u8) = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);

        let (expected, text) = (("test", 39, ('p', 19.0)), r#"("test", 39, ('p', 19.0))"#);
        let deserialized: (&str, u8, (char, f64)) = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);
    }

    #[test]
    pub fn tuple_struct() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct TupleStruct(u8, char);

        let values = [
            (TupleStruct(10, 'a'), r"TupleStruct(10, 'a')"),
            (TupleStruct(27, 'b'), r"TupleStruct ( 27 , 'b' ) "),
            (TupleStruct(111, '\n'), r"TupleStruct(111, '\n',)"),
        ];

        for (expected, text) in values.iter() {
            let deserialized: TupleStruct =
                from_str(text).expect(&format!("Could not deserialize {}", text));
            assert_eq!(deserialized, *expected);
        }
    }

    #[test]
    fn map() {
        let (expected, text): (HashMap<&str, u8>, &str) = (
            [("a", 29), ("b", 100)].iter().map(|x| *x).collect(),
            r#"[
    "a": 29,
    "b": 100,
]"#,
        );
        let deserialized: HashMap<&str, u8> = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);

        let (expected, text): (HashMap<u16, String>, &str) = (
            [(10, "10"), (3502, "3502"), (20, "\n")]
                .iter()
                .map(|x| (x.0, x.1.to_string()))
                .collect(),
            r#"[
    10: "10",
    3502: "3502",
    20: "\n",
]"#,
        );
        let deserialized: HashMap<u16, String> = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);

        // Nested.......
        let mut map: HashMap<Vec<u64>, HashMap<u8, &str>> = HashMap::new();
        map.insert(
            vec![10, 100, 1000],
            [(70, "70"), (50, "50")].iter().map(|x| *x).collect(),
        );
        map.insert(
            vec![20, 200, 2000],
            [(10, "90"), (11, "10")].iter().map(|x| *x).collect(),
        );
        let (expected, text): (HashMap<Vec<u64>, HashMap<u8, &str>>, &str) = (
            map,
            r#"[
    [10, 100, 1000]: [70: "70", 50: "50"],
    [20, 200, 2000]: [10: "90", 11: "10"],
]"#,
        );
        let deserialized: HashMap<Vec<u64>, HashMap<u8, &str>> = from_str(&text).unwrap();
        assert_eq!(deserialized, expected);
    }

    #[test]
    pub fn deserialize_any() {
        use Enum::*;

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        #[serde(untagged)]
        enum Enum {
            Bool(bool),
            U8(u8),
            I8(i8),
            U16(u16),
            I16(i16),
            U32(u32),
            I32(i32),
            U64(u64),
            I64(i64),
            // Unfortunately serde's untagged enums do not support 128-bit
            // integers: https://github.com/serde-rs/serde/issues/1717
            // U128(u128),
            // I128(i128),
            F64(f64),
            Char(char),
            StringVariant(String),
            Sequence([u8; 4]),
            Map(HashMap<i32, i32>),
            Unit(()),
            Tuple((i32, i32)),
            OptionVariant(Option<i32>),
            StructVariant(Struct),
            UnitStructVariant(UnitStruct),
            TupleStructVariant(TupleStruct),
        }

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Struct {
            a: i32,
            b: i32,
        }
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct UnitStruct;
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct TupleStruct(i32, i32, i32);

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Variants {
            bool: Enum,
            u8: Enum,
            i8: Enum,
            u16: Enum,
            i16: Enum,
            u32: Enum,
            i32: Enum,
            u64: Enum,
            i64: Enum,
            f64: Enum,
            char: Enum,
            string: Enum,
            sequence: Enum,
            map: Enum,
            unit: Enum,
            tuple: Enum,
            option_some: Enum,
            option_none: Enum,
            struct_variant: Enum,
            tuple_struct: Enum,
        }

        let expected = Variants {
            bool: Bool(false),
            u8: U8(255),
            i8: I8(-128),
            u16: U16(65_535),
            i16: I16(-32_768),
            u32: U32(4_294_967_295),
            i32: I32(-2_147_483_648),
            u64: U64(18_446_744_073_709_551_615),
            i64: I64(-9_223_372_036_854_775_808),
            f64: F64(420.69),
            char: Char('A'),
            string: StringVariant("OwO".to_owned()),
            sequence: Sequence([1, 1, 2, 3]),
            map: Map([(-1, 29), (1, 100)].iter().map(|x| *x).collect()),
            unit: Unit(()),
            tuple: Tuple((17, 06)),
            option_some: OptionVariant(Some(1337)),
            option_none: OptionVariant(None),
            struct_variant: StructVariant(Struct { a: 12, b: 24 }),
            tuple_struct: TupleStructVariant(TupleStruct(7, 27, 37)),
        };

        let text = r#"Variants {
    bool: false,
    u8: 255,
    i8: -128,
    u16: 65_535,
    i16: -32_768,
    u32: 4_294_967_295,
    i32: -2_147_483_648,
    u64: 18_446_744_073_709_551_615,
    i64: -9_223_372_036_854_775_808,
    f64: 420.69,
    char: 'A',
    string: "OwO",
    sequence: [1, 1, 2, 3],
    map: [-1: 29, 1: 100],
    unit: (),
    tuple: (17, 06),
    option_some: Some(1337),
    option_none: None,
    struct_variant: Struct { a: 12, b: 24 },
    tuple_struct: TupleStruct(7, 27, 37),
}"#;
        assert_eq!(from_str::<Variants>(text).unwrap(), expected);
    }

    // Some variants are deserialized in the exact same way in serde's untagged enums, so we can't
    // add both in the same enum. f32 is the same as f64 and Unit Struct is the same as Unit.
    #[test]
    pub fn deserialize_any_2() {
        use Enum::*;

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        #[serde(untagged)]
        enum Enum {
            F32(f32),
            UnitStructVariant(UnitStruct),
        }

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct UnitStruct;

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Variants {
            f32: Enum,
            unit_struct_1: Enum,
            unit_struct_2: Enum,
        }

        let expected = Variants {
            f32: F32(69.420),
            unit_struct_1: UnitStructVariant(UnitStruct),
            unit_struct_2: UnitStructVariant(UnitStruct {}),
        };

        let text = r#"Variants {
    f32: 69.420,
    unit_struct_1: UnitStruct,
    unit_struct_2: UnitStruct {},
}"#;
        assert_eq!(from_str::<Variants>(text).unwrap(), expected);
    }
}
