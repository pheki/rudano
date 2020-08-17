//! Utilities for serializing data structures into Rudano.

use std::fmt::Write;

use serde::{ser, Serialize};

use crate::error::{SerializationError as Error, SerializationResult as Result};

const DEFAULT_INDENT: &str = "    ";
const MAX_ELEMENTS_SINGLELINE_SEQ: usize = 4;

/// Serializes the given `value` as a String of compact Rudano.
///
/// The compact representation of Rudano includes no whitespace or trailing comma.
/// If you're looking for pretty-printed Rudano, use `to_string_pretty` instead.
///
/// # Example
///
/// ```
/// use serde::{Serialize, Deserialize};
///
/// #[derive(Serialize, Deserialize)]
/// struct MyStruct {
///     number: i32,
///     string: String,
/// }
///
/// let value = MyStruct {
///     number: 69,
///     string: "nice".to_string(),
/// };
/// let string = rudano::to_string_compact(&value).unwrap();
///
/// let expected = r#"MyStruct{number:69,string:"nice"}"#;
///
/// assert_eq!(string, expected);
/// ```
///
/// # Errors
///
/// Serialization can fail if the structure's Serialize implementation fails.
pub fn to_string_compact<T: Serialize>(value: &T) -> Result<String> {
    let mut serializer = Serializer::compact();
    value.serialize(&mut serializer)?;
    Ok(serializer.finish())
}

/// Serializes the given `value` as a String of pretty-printed Rudano.
///
/// This will generate newlines, indentation, etc.
/// If you're looking for generating compact Rudano, use `to_string_compact` instead.
///
/// # Example
///
/// ```
/// use serde::{Serialize, Deserialize};
///
/// #[derive(Serialize, Deserialize)]
/// struct MyStruct {
///     number: i32,
///     string: String,
/// }
///
/// let value = MyStruct {
///     number: 69,
///     string: "nice".to_string(),
/// };
/// let string = rudano::to_string_pretty(&value).unwrap();
///
/// let expected = r#"MyStruct {
///     number: 69,
///     string: "nice",
/// }"#;
///
/// assert_eq!(string, expected);
/// ```
///
/// # Errors
///
/// Serialization can fail if the structure's Serialize implementation or itoa fails.
pub fn to_string_pretty<T: Serialize>(value: &T) -> Result<String> {
    let mut serializer = Serializer::pretty();
    value.serialize(&mut serializer)?;
    Ok(serializer.finish())
}

struct PrettyData {
    indentation_level: u32,
    indentation_str: &'static str,
}

impl PrettyData {
    fn indentation_level_up(&mut self) {
        self.indentation_level = self.indentation_level.saturating_add(1);
    }

    fn indentation_level_down(&mut self) {
        self.indentation_level = self.indentation_level.saturating_sub(1);
    }

    fn indent(&mut self, output: &mut String) {
        for _ in 0..self.indentation_level {
            output.push_str(self.indentation_str);
        }
    }
}

/// Rudano's serializer. Its recommended that you use `to_string_compact` or `to_string_pretty`
/// instead.
pub struct Serializer {
    output: String,
    pretty: Option<PrettyData>,
}

impl Serializer {
    /// Creates a new Serializer using pretty-printed representation and default indent (4 spaces
    /// per level).
    pub fn pretty() -> Serializer {
        Serializer::with_indentation_str(DEFAULT_INDENT)
    }

    /// Creates a new Serializer using compact representation.
    pub fn compact() -> Serializer {
        Serializer {
            output: String::new(),
            pretty: None,
        }
    }

    /// Creates a new Serializer using pretty-printed representation and custom indent per level.
    pub fn with_indentation_str(indentation_str: &'static str) -> Serializer {
        Serializer {
            output: String::new(),
            pretty: Some(PrettyData {
                indentation_level: 0,
                indentation_str,
            }),
        }
    }

    /// Finish running the serializer, returning its output.
    pub fn finish(self) -> String {
        self.output
    }

    #[allow(unused)]
    fn last_semantic_char(&self) -> Option<char> {
        self.output.chars().rev().find(|c| !c.is_whitespace())
    }
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();

    type Error = Error;

    type SerializeSeq = SeqSerializer<'a>;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<()> {
        self.output += if v { "true" } else { "false" };
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_i16(self, v: i16) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_i32(self, v: i32) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_i64(self, v: i64) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_i128(self, v: i128) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_u16(self, v: u16) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_u32(self, v: u32) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_u64(self, v: u64) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_u128(self, v: u128) -> Result<()> {
        itoa::fmt(&mut self.output, v)?;
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<()> {
        // Checks if float has no fractionary part
        if v.fract() == 0.0 {
            // Prints with single precision (to look like a float)
            write!(self.output, "{:.1}", v)?;
        } else {
            // Prints with full precision
            write!(self.output, "{}", v)?;
        }
        Ok(())
    }

    fn serialize_f64(self, v: f64) -> Result<()> {
        // Checks if float has no fractionary part
        if v.fract() == 0.0 {
            // Prints with single precision (to look like a float)
            write!(self.output, "{:.1}", v)?;
        } else {
            // Prints with full precision
            write!(self.output, "{}", v)?;
        }
        Ok(())
    }

    fn serialize_char(self, v: char) -> Result<()> {
        self.output.push('\'');
        self.output += &v.escape_default().to_string();
        self.output.push('\'');
        Ok(())
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        self.output.push('"');
        self.output += &v.escape_default().to_string();
        self.output.push('"');
        Ok(())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<()> {
        use serde::ser::SerializeSeq;
        let mut seq = self.serialize_seq(Some(v.len()))?;
        for byte in v {
            seq.serialize_element(byte)?;
        }
        seq.end()
    }

    fn serialize_none(self) -> Result<()> {
        self.output += "None";
        Ok(())
    }

    fn serialize_some<T>(self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.output += "Some(";
        value.serialize(&mut *self)?;
        self.output += ")";
        Ok(())
    }

    fn serialize_unit(self) -> Result<()> {
        self.output += "()";
        Ok(())
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<()> {
        self.output += name;
        if self.pretty.is_some() {
            self.output += " ";
        }
        self.output += "{}";
        Ok(())
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<()> {
        self.output += variant;
        Ok(())
    }

    fn serialize_newtype_struct<T>(self, name: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.output += name;
        self.output += "(";
        value.serialize(&mut *self)?;
        self.output += ")";
        Ok(())
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<()> {
        self.output += variant;
        self.output += "(";
        value.serialize(&mut *self)?;
        self.output += ")";
        Ok(())
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.output.push('[');
        if let Some(ref mut pretty) = self.pretty {
            match len {
                Some(0..=MAX_ELEMENTS_SINGLELINE_SEQ) => Ok(SeqSerializer {
                    serializer: self,
                    format: SeqFormat::SingleLine,
                }),
                _ => {
                    self.output.push('\n');
                    pretty.indentation_level_up();
                    Ok(SeqSerializer {
                        serializer: self,
                        format: SeqFormat::MultiLine,
                    })
                }
            }
        } else {
            Ok(SeqSerializer {
                serializer: self,
                format: SeqFormat::SingleLine,
            })
        }
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        self.output += "(";
        Ok(self)
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.output += name;
        self.output += "(";
        Ok(self)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        self.output += variant;
        self.output += "(";
        Ok(self)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        self.output.push('[');
        if let Some(ref mut pretty) = self.pretty {
            self.output.push('\n');
            pretty.indentation_level_up();
        }
        Ok(self)
    }

    fn serialize_struct(self, name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        self.output.push_str(name);
        if let Some(ref mut pretty) = self.pretty {
            self.output += " {\n";
            pretty.indentation_level_up();
        } else {
            self.output += "{";
        }
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        self.output += variant;
        if let Some(ref mut pretty) = self.pretty {
            self.output += " {\n";
            pretty.indentation_level_up();
        } else {
            self.output += "{";
        }
        Ok(self)
    }
}

#[doc(hidden)]
pub enum SeqFormat {
    SingleLine,
    MultiLine,
}

#[doc(hidden)]
pub struct SeqSerializer<'a> {
    serializer: &'a mut Serializer,
    format: SeqFormat,
}

impl<'a> ser::SerializeSeq for SeqSerializer<'_> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let Some(ref mut pretty) = self.serializer.pretty {
            match self.format {
                SeqFormat::SingleLine => {
                    if !self.serializer.output.ends_with('[') {
                        self.serializer.output += ", ";
                    }
                    value.serialize(&mut *self.serializer)?;
                }
                SeqFormat::MultiLine => {
                    pretty.indent(&mut self.serializer.output);
                    value.serialize(&mut *self.serializer)?;
                    self.serializer.output += ",\n";
                }
            }
        } else {
            if !self.serializer.output.ends_with('[') {
                self.serializer.output += ",";
            }
            value.serialize(&mut *self.serializer)?;
        }
        Ok(())
    }

    fn end(self) -> Result<()> {
        if let (SeqFormat::MultiLine, Some(pretty)) = (self.format, self.serializer.pretty.as_mut())
        {
            pretty.indentation_level_down();
            pretty.indent(&mut self.serializer.output);
        }
        self.serializer.output += "]";
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if !self.output.ends_with('(') {
            self.output += ",";
            if self.pretty.is_some() {
                self.output += " ";
            }
        }
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.output += ")";
        Ok(())
    }
}

impl<'a> ser::SerializeTupleStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if !self.output.ends_with('(') {
            self.output += ",";
            if self.pretty.is_some() {
                self.output += " ";
            }
        }
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.output += ")";
        Ok(())
    }
}

impl<'a> ser::SerializeTupleVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if !self.output.ends_with('(') {
            self.output += ",";
            if self.pretty.is_some() {
                self.output += " ";
            }
        }
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.output += ")";
        Ok(())
    }
}

impl<'a> ser::SerializeMap for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let Some(ref mut pretty) = self.pretty {
            pretty.indent(&mut self.output);
            key.serialize(&mut **self)?;
            self.output += ": ";
        } else {
            if !self.output.ends_with('[') {
                self.output += ",";
            }
            key.serialize(&mut **self)?;
        }

        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if self.pretty.is_some() {
            value.serialize(&mut **self)?;
            self.output += ",\n";
        } else {
            self.output += ":";
            value.serialize(&mut **self)?;
        }
        Ok(())
    }

    fn end(self) -> Result<()> {
        if let Some(ref mut pretty) = self.pretty {
            pretty.indentation_level_down();
            pretty.indent(&mut self.output);
        }
        self.output += "]";

        Ok(())
    }
}

impl<'a> ser::SerializeStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let Some(ref mut pretty) = self.pretty {
            pretty.indent(&mut self.output);
            self.output += key;
            self.output += ": ";
            value.serialize(&mut **self)?;
            self.output += ",\n";
        } else {
            if !self.output.ends_with('{') {
                self.output += ",";
            }
            self.output += key;
            self.output += ":";
            value.serialize(&mut **self)?;
        }
        Ok(())
    }

    fn end(self) -> Result<()> {
        if let Some(ref mut pretty) = self.pretty {
            pretty.indentation_level_down();
            pretty.indent(&mut self.output);
            self.output += "}";
        } else {
            self.output += "}";
        }
        Ok(())
    }
}

impl<'a> ser::SerializeStructVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let Some(ref mut pretty) = self.pretty {
            pretty.indent(&mut self.output);
            self.output += key;
            self.output += ": ";
            value.serialize(&mut **self)?;
            self.output += ",\n";
        } else {
            if !self.output.ends_with('{') {
                self.output += ",";
            }
            self.output += key;
            self.output += ":";
            value.serialize(&mut **self)?;
        }
        Ok(())
    }

    fn end(self) -> Result<()> {
        if let Some(ref mut pretty) = self.pretty {
            pretty.indentation_level_down();
            pretty.indent(&mut self.output);
        }
        self.output += "}";
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ser::{to_string_compact, to_string_pretty};
    use serde::{Deserialize, Serialize};
    use std::collections::BTreeMap;

    fn is_trailing_comma(c: char) -> bool {
        c == '\u{201A}'
    }

    fn translate_commas(s: &str) -> String {
        s.chars()
            .map(|c| if is_trailing_comma(c) { ',' } else { c })
            .collect()
    }

    fn remove_whitespace_commas(s: &str) -> String {
        let mut inside_string = false;
        s.chars()
            .filter(|&c| {
                if c == '"' {
                    inside_string = !inside_string;
                }
                if !inside_string {
                    !c.is_whitespace() && !is_trailing_comma(c)
                } else {
                    true
                }
            })
            .collect()
    }

    #[test]
    fn boolean() {
        let values = [(true, r"true"), (false, r"false")];

        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn uint8() {
        let random: u8 = rand::random();
        let string = random.to_string();
        let values = [
            (27u8, r"27"),
            (43, r"43"),
            (std::u8::MIN, r"0"),
            (std::u8::MAX, r"255"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn uint16() {
        let random: u16 = rand::random();
        let string = random.to_string();
        let values = [
            (27u16, r"27"),
            (43, r"43"),
            (std::u16::MIN, r"0"),
            (std::u16::MAX, r"65535"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn uint32() {
        let random: u32 = rand::random();
        let string = random.to_string();
        let values = [
            (27u32, r"27"),
            (43, r"43"),
            (std::u32::MIN, r"0"),
            (std::u32::MAX, r"4294967295"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn uint64() {
        let random: u64 = rand::random();
        let string = random.to_string();
        let values = [
            (27u64, r"27"),
            (43, r"43"),
            (std::u64::MIN, r"0"),
            (std::u64::MAX, r"18446744073709551615"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn uint128() {
        let random: u128 = rand::random();
        let string = random.to_string();
        let values = [
            (27u128, r"27"),
            (43, r"43"),
            (std::u128::MIN, r"0"),
            (std::u128::MAX, r"340282366920938463463374607431768211455"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn int8() {
        let random: i8 = rand::random();
        let string = random.to_string();
        let values = [
            (27i8, r"27"),
            (43, r"43"),
            (-37, r"-37"),
            (std::i8::MIN, r"-128"),
            (std::i8::MAX, r"127"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn int16() {
        let random: i16 = rand::random();
        let string = random.to_string();
        let values = [
            (27i16, r"27"),
            (43, r"43"),
            (-37, r"-37"),
            (std::i16::MIN, r"-32768"),
            (std::i16::MAX, r"32767"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn int32() {
        let random: i32 = rand::random();
        let string = random.to_string();
        let values = [
            (27i32, r"27"),
            (43, r"43"),
            (-37, r"-37"),
            (std::i32::MIN, r"-2147483648"),
            (std::i32::MAX, r"2147483647"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn int64() {
        let random: i64 = rand::random();
        let string = random.to_string();
        let values = [
            (27i64, r"27"),
            (43, r"43"),
            (-37, r"-37"),
            (std::i64::MIN, r"-9223372036854775808"),
            (std::i64::MAX, r"9223372036854775807"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn int128() {
        let random: i128 = rand::random();
        let string = random.to_string();
        let values = [
            (27i128, r"27"),
            (43, r"43"),
            (-37, r"-37"),
            (std::i128::MIN, r"-170141183460469231731687303715884105728"),
            (std::i128::MAX, r"170141183460469231731687303715884105727"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn float32() {
        let random: f32 = rand::random();
        let string = random.to_string();
        let values = [
            (27f32, r"27.0"),
            (43.0, r"43.0"),
            (239.34, r"239.34"), // Floating point comparison problems
            (-37.3, r"-37.3"),
            (-37.0E+12, r"-36999998210048.0"),
            (-92., r"-92.0"),
            (std::f32::INFINITY, r"inf"),
            (std::f32::NEG_INFINITY, r"-inf"),
            (std::f32::NAN, r"NaN"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn float64() {
        let random: f64 = rand::random();
        let string = random.to_string();
        let values = [
            (27f64, r"27.0"),
            (43.0, r"43.0"),
            (239.34, r"239.34"),
            (-37.3, r"-37.3"),
            (-37.0E+12, r"-37000000000000.0"),
            (-92., r"-92.0"),
            (std::f64::INFINITY, r"inf"),
            (std::f64::NEG_INFINITY, r"-inf"),
            (std::f64::NAN, r"NaN"),
            (random, &string),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    #[test]
    fn character() {
        let values = [
            ('a', r"'a'"),
            ('@', r"'@'"),
            ('\u{bf0}', r"'\u{bf0}'"),
            ('\u{1f638}', r"'\u{1f638}'"),
            ('😸', r"'\u{1f638}'"), // Serialized as unicode
            ('\n', r"'\n'"),
            ('\t', r"'\t'"),
            (std::char::MAX, r"'\u{10ffff}'"),
            (std::char::REPLACEMENT_CHARACTER, r"'\u{fffd}'"),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
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
            ("\u{bf0}\u{1f638}", r#""\u{bf0}\u{1f638}""#),
            ("😸", r#""\u{1f638}""#), // Serialized as unicode
            ("such\nwow", r#""such\nwow""#),
            ("very\tlol", r#""very\tlol""#),
            ("\u{10ffff}", r#""\u{10ffff}""#),
            ("\u{fffd}", r#""\u{fffd}""#),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }
    }

    // NOTE: Using U+201A instead of U+002C for trailing commas, so they can be removed on compact serialization tests...
    #[test]
    fn byte_array() {
        let values: [(&[u8], &str); 3] = [
            (&[], r"[]"),
            (&[203, 100, 21, 9], r"[203, 100, 21, 9]"),
            (
                &[12, 49, 129, 20, 30, 213],
                r"[
    12,
    49,
    129,
    20,
    30,
    213‚
]",
            ),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(
                to_string_pretty(&value).unwrap(),
                translate_commas(expected)
            );
            assert_eq!(
                to_string_compact(&value).unwrap(),
                remove_whitespace_commas(expected)
            );
        }
    }

    #[test]
    fn option() {
        let values = [
            (None, "None".to_string()),
            (Some(()), "Some(())".to_string()),
        ];
        for (value, expected) in values.iter() {
            assert_eq!(&to_string_pretty(&value).unwrap(), expected);
            assert_eq!(&to_string_compact(&value).unwrap(), expected);
        }

        assert_eq!(&to_string_pretty(&Some(30u8)).unwrap(), "Some(30)");
        assert_eq!(&to_string_compact(&Some(30u8)).unwrap(), "Some(30)");
    }

    #[test]
    fn unit() {
        let (value, expected) = ((), "()");
        assert_eq!(&to_string_pretty(&value).unwrap(), expected);
        assert_eq!(&to_string_compact(&value).unwrap(), expected);
    }

    #[test]
    fn unit_struct() {
        #[derive(PartialEq, Serialize, Deserialize, Debug)]
        struct Unit;

        let (value, expected) = (Unit, "Unit {}");

        assert_eq!(&to_string_pretty(&value).unwrap(), expected);
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );
    }

    #[test]
    pub fn newtype_struct() {
        #[derive(PartialEq, Serialize, Deserialize, Debug)]
        struct Newtype(u8);

        let (value, expected) = (Newtype(27), "Newtype(27)");

        assert_eq!(&to_string_pretty(&value).unwrap(), expected);
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );
    }

    // NOTE: Using U+201A instead of U+002C for trailing commas, so they can be removed on compact serialization tests...
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
            newtype: Enum,
            tuple: Enum,
            struct_variant: Enum,
        }

        let value = Variants {
            unit: Unit,
            newtype: Newtype(70),
            tuple: Tuple(20, 80),
            struct_variant: Struct {
                a: 10,
                b: [2, 4, 8],
            },
        };

        let expected = r#"Variants {
    unit: Unit,
    newtype: Newtype(70),
    tuple: Tuple(20, 80),
    struct_variant: Struct {
        a: 10,
        b: [2, 4, 8]‚
    }‚
}"#;

        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );
    }

    // NOTE: Using U+201A instead of U+002C for trailing commas, so they can be removed on compact serialization tests...
    #[test]
    fn seq() {
        let (value, expected): (Vec<u8>, _) = (vec![], r"[]");
        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );

        let (value, expected): (Vec<u8>, _) = (vec![203, 100, 21, 9], r"[203, 100, 21, 9]");
        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );

        let (value, expected) = (
            vec![12, 49, 129, 20, 30, 213],
            r"[
    12,
    49,
    129,
    20,
    30,
    213‚
]",
        );
        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );

        let (value, expected) = (
            vec!['o', '\n', '1', '\t', '😸', '\u{1f638}'],
            r"[
    'o',
    '\n',
    '1',
    '\t',
    '\u{1f638}',
    '\u{1f638}'‚
]",
        );
        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );

        let (value, expected) = (vec![vec![20.3, 19.2, -100.22]], r"[[20.3, 19.2, -100.22]]");
        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );
    }

    // NOTE: Using U+201A instead of U+002C for trailing commas, so they can be removed on compact serialization tests...
    #[test]
    fn tuple() {
        // Actually unit type, which is an empty tuple
        let (value, expected) = ((), r"()");
        assert_eq!(&to_string_pretty(&value).unwrap(), expected);
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );

        let (value, expected) = ((12, 49, 129, 20, 30, 213), r"(12, 49, 129, 20, 30, 213)");
        assert_eq!(&to_string_pretty(&value).unwrap(), expected);
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );

        let (value, expected) = (("test", 39, ('p', 19.0)), r#"("test", 39, ('p', 19.0))"#);
        assert_eq!(&to_string_pretty(&value).unwrap(), expected);
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );
    }

    #[test]
    fn tuple_struct() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct TupleStruct(u8, char);

        let (value, expected) = (TupleStruct(10, 'a'), r"TupleStruct(10, 'a')");
        assert_eq!(&to_string_pretty(&value).unwrap(), expected);
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );
    }

    #[test]
    fn map() {
        // Using BTreeMap because serialization order is then predictable
        let (value, expected): (BTreeMap<&str, u8>, &str) = (
            [("a", 29), ("b", 100)].iter().map(|x| *x).collect(),
            r#"[
    "a": 29,
    "b": 100‚
]"#,
        );
        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );

        let (value, expected): (BTreeMap<u16, String>, &str) = (
            [(10, "10"), (3502, "3502"), (20, "\n")]
                .iter()
                .map(|x| (x.0, x.1.to_string()))
                .collect(),
            r#"[
    10: "10",
    20: "\n",
    3502: "3502"‚
]"#,
        );
        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );

        // Nested.......
        let mut map: BTreeMap<Vec<u64>, BTreeMap<u8, &str>> = BTreeMap::new();
        map.insert(
            vec![10, 100, 1000],
            [(70, "70"), (50, "50")].iter().map(|x| *x).collect(),
        );
        map.insert(
            vec![20, 200, 2000],
            [(10, "90"), (11, "10")].iter().map(|x| *x).collect(),
        );
        let (value, expected): (BTreeMap<Vec<u64>, BTreeMap<u8, &str>>, &str) = (
            map,
            r#"[
    [10, 100, 1000]: [
        50: "50",
        70: "70"‚
    ],
    [20, 200, 2000]: [
        10: "90",
        11: "10"‚
    ]‚
]"#,
        );
        assert_eq!(
            to_string_pretty(&value).unwrap(),
            translate_commas(expected)
        );
        assert_eq!(
            to_string_compact(&value).unwrap(),
            remove_whitespace_commas(expected)
        );
    }
}
