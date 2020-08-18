# Rudano

[![Build Status](https://travis-ci.org/pheki/rudano.svg?branch=master)](https://travis-ci.org/pheki/rudano)
[![Crate](https://img.shields.io/crates/v/rudano.svg)](https://crates.io/crates/rudano)
[![API](https://docs.rs/rudano/badge.svg)](https://docs.rs/rudano)
[![Minimum rustc version](https://img.shields.io/badge/rustc-1.40+-green.svg)](https://github.com/pheki/rudano#rust-version-support)

Rudano, Rust Data Notation, is a data serialization format designed to be as
similar as possible to Rust's own syntax.

# Why?

While I really like [RON](https://github.com/ron-rs/ron), I find that syntax
differences make it confusing when you're switching contexts between it and Rust
a lot, specially as () are used for structs.

That's why I created Rudano, with the focus on being very similar to Rust's
literal notation itself, so that you can just write your "object" descriptions
using the same syntax you're already used to.

The exception is maps, which do not have a literal syntax in Rust, and have been
chosen to be delimited with `[]`. Its explained why in [its spec](#map).

# Example and Comparison

Consider the following imaginary rust structure:

```rust
#[derive(Serialize, Deserialize)]
enum Ip {
    V4(u8, u8, u8, u8),
    V6(u8, u8, u8, u8, u8, u8, u8, u8),
}

#[derive(Serialize, Deserialize)]
struct Network {
    name: String,
    local_address: Ip,
    hosts: HashMap<String, Ip>,
}
```

An example of this structure in Rudano:

```rust
// Struct name is required (see why below)
Network {
    name: "Local Network",
    // Enums are supported
    local_address: V4(192, 168, 0, 100),
    hosts: [
        "Foo": V6(0, 0, 0, 0, 0, 0, 0, 0xA3),
        "Bar": V4(192, 168, 0, 104),
    ], // Trailing comma is allowed
}
```

Same example in Ron:

```
// Struct name is optional
Network (
    name: "Local Network",
    // Enums are supported
    local_address: V4(192, 168, 0, 100),
    hosts: {
        "Foo": V6(0, 0, 0, 0, 0, 0, 0, 0xA3),
        "Bar": V4(192, 168, 0, 104),
    }, // Trailing comma is allowed
)
```

In JSON, comments are not allowed, but I hope it speaks by itself:

```
{
    "name": "Local Network",
    "local_address": {
        "V4": [192, 168, 0, 100]
    },
    "hosts": {
        "Foo": {
            "V6": [0, 0, 0, 0, 0, 0, 0, 0xA3]
        },
        "Bar": {
            "V4": [192, 168, 0, 104]
        }
    }
}
```

|         | Rudano |  RON  |  JSON  |
| ------- | :----: | :---: | :----: |
| Comments | // | // and /* */ | Not allowed |
| Structs | Name { property: value } | Name ( property: value ) | { "property": value } |
| Arrays | [value] | [value] | [value] |
| Maps | [key: value] | { key: value } | { key: value } |
| Enums | Variant(data) | Variant(data) | { "Variant": data } |
| Trailing Comma | Allowed | Allowed | Not allowed |

# Contributing

Any question, bug report, suggestion or pull request is welcome! Just open an
issue or PR.

# Stability

Unstable, the format itself may change to fix ambiguity issues and conform
better to Rust's syntax, using semver for breaking changes.

# Caveats

- Zero-copy deserialization with borrowed strings will fail if escape codes are
used. (I believe it's also true for any other format)

# Why are struct names required?

As structs names are required in the format, structs can be later converted into
an enum variant without breaking backward compatibility. For example, when using

```rust
struct Point { position: (i32, i32), name: String }

enum Object {
    Point { position: (i32, i32), name: String },
    Canvas { size: (i32, i32) }
}
```

The serialized value `Point { position: (10, 20), "Great Point!" }` is valid
both as a Point and an Object. This is valid for all struct types and their
equivalent enum variants (Unit, NewType, Tuple and Struct).

# Rust version support

The MSRV is **1.40 or greater** and its tested with 1.40.0, stable, beta and
nightly. Changing it will only be done in a minor (0.x) or greater release.

# Data Types

Notice that in all examples with definitions, `#[derive(Serialize, Deserialize)]` is omitted for brevity.

## Bool

Bool types may be either the literals `true` or `false`.

### Examples

- `true`
- `false`

## Signed Integers

Signed integers includes i8, i16, i32, i64, and i128. They start with `-` if they are negative, but not `+` if they're positive. `_` (underscores) in the number are ignored and they may be written in hexadecimal, octal or binary form, by using the prefixes `0x`, `0o`, or `0b` respectively.

### Examples

For i16:

- `27`
- `43`
- `0x2A`
- `-0x1F`
- `0b11010`
- `-37`
- `-32768`
- `32767`

## Unsigned integers

Unsigned integers includes u8, u16, u32, u64, and u128. They do not support `-` or `+`. `_` (underscores) in the number are ignored and they may be written in hexadecimal, octal or binary form, by using the prefixes `0x`, `0o`, or `0b` respectively.

### Examples

For u16:

- `27`
- `43`
- `0x2A`
- `0b11010`
- `0`
- `65535`

## Floats

Float types includes f32 and f64, and they are not yet fully specified.

### Examples

For f32:

- `27`
- `43.0`
- `239.34`
- `-37.3`
- `-37.0E+12`
- `-92`
- `inf`
- `-inf`

## Char

Char types are delimited by `''` and may include any character or escape code as defined in [the reference](https://doc.rust-lang.org/reference/tokens.html?highlight=escape#character-and-string-literals).

### Examples

- `'a'`
- `'@'`
- `'\x5A'`
- `'\u{bf0}'`
- `'\u{1f638}'`
- `'😸'`
- `'\n'`
- `'\t'`
- `'\u{10ffff}'`
- `'\u{fffd}'`

## String

String types are delimited by `""` and may include any character or escape code as defined in [the reference](https://doc.rust-lang.org/reference/tokens.html?highlight=escape#character-and-string-literals).

### Examples

- `"a"`
- `"@"`
- `"The quick brown fox jumps over the lazy dog"`
- `"\x5A"`
- `"\u{bf0}\u{1f638}"`
- `"😸"`
- `"such\nwow"`
- `"very\tlol"`
- `"\u{10ffff}"`
- `"\u{fffd}"`

## Option

Options types have two possiblities: it may be the identifier `None` or the identifier `Some`, which includes parenthesis and a value after.

### Examples

- `None`
- `Some(())`
- `Some(30)`

## Unit

Unit types include only a single possible value: `()`.

Example:

- `()`

## Unit Struct

Unit struct is a named value with a single value: the struct itself. It is expressed as the struct name and optionally empty curly braces (`{}`).

### Examples

Definition

`struct Unit;`

Serialized

- `Unit`
- `Unit {}`

## Newtype Struct

Newtype structs are structs with a single, unnamed field. It is represented by the struct name, parenthesis and the value. It is actually a specific case of a tuple struct. If you want it to be represented by just its internal value, use [`#[serde(transparent)]`](https://serde.rs/container-attrs.html#transparent).

### Examples

Definition

```rust
struct Newtype(u8);
```

Serialized

- `Newtype(39)`
- `Newtype(0xa8)`

Definition

```rust
#[serde(transparent)]
struct Transparent(String);
```

Serialized

- `"yay"`
- `"it works!"`

## Tuple Struct

Tuples structs are structs with multiple unnamed fields, akin to named tuples. Its represented by the struct name and the values inside parenthesis.

### Example

Definition

```rust
struct TupleStruct(u8, char);
```

Serialized

- `TupleStruct(10, 'a')`

## Struct

Structs are structs with named fields. Represented by struct name, followed by curly braces and comma separated `identifier: value` pairs. Trailing comma is allowed (and recommended for multi-line).

### Example

Definition

```rust
struct Point { position: (i32, i32), name: String }
```

Serialized

- `Point { position: (10, 20), "Great Point!" }`

## Enum Variants

The enum variants are serialized and deserialized exactly the same as their struct counterparts. The variant types are: Unit, NewType, Tuple and Struct.

### Example

Definition

```
enum Enum {
    Unit,
    Newtype(u32),
    Tuple(u32, u32),
    Struct {
        a: u32,
    },
}

struct Variants {
    unit: Enum,
    newtype: Enum,
    tuple: Enum,
    struct_variant: Enum,
}
```

Serialized structs with all variants:

```rust
Variants {
    unit: Unit,
    newtype: Newtype(70),
    tuple: Tuple(20, 80),
    struct_variant: Struct {
        a: 10,
    },
}
```
## Sequence (or array)

An array is a contiguous sequence of elements of the same type. Represented by a comma-separated list of its elements surounded by square braces. Automatically derived by serde for Vec, can actually be used for arrays by adding #[serde(serialize_with = "rudano::serialize_array")] to the field definition.

### Example

- `[10, 20, 30]`
- `["abc", "wow"]`

## Map

An map is a set of (key, value) pairs. Represented by sequence of comma-separated pairs, each pair having a colon (`:`) between its key and its value. Automatically derived by serde for HashMap and other std map types.

### Example

Definition

`HashMap<String, i32>`

Serialized

- `["x": 10, "y": 20]`
- `["blue": 21, "red": 22]`

Definition

`HashMap<i32, String>`

- `[1: "One"]`
- `[13: "Too high"]`

### Rationale

Maps are delimited in rudano with `[]`. The reason is that maps are homogeneous
structures (like arrays, but unlike structures) and they can actually be
initialized in Rust code with `[(key0, value0), (key1, value1)].collect()`,
which is similar to RuDaNo's `[key0: value0, key1: value1]`. It also makes maps
different than structs, which was one of RON's original motivations.

# Differences with Rust's Syntax

These differences are intentional (but subject to discussion):
- Float types without decimal point are allowed
- Numbers with type suffixes are not supported (e.g. 15u64)
- Vecs are serialized as `[1, 2, 3]`, not `vec![1, 2, 3]`
- Arrays are serialized as tuples by default (because that's the default Serialize impl). Using `#[serde(serialize_with = "rudano::serialize_array")]` will make it serialize using array notation (as an `[]` sequence)

# License

This crate (library) is distributed under terms of MIT license or Apache License (Version 2.0), at your option.
See `LICENSE-MIT` and `LICENSE-APACHE` for terms.
