use serde::ser::{Serialize, SerializeSeq, Serializer};

/// Serializes array as sequence instead of serde's derive default (tuple).
///
/// # Example
///
/// ```
/// use serde::{Serialize, Deserialize};
///
/// #[derive(Serialize, Deserialize)]
/// struct MyStruct {
///     #[serde(serialize_with = "rudano::serialize_array")]
///     my_array: [u8; 4],
/// }
///
/// let value = MyStruct {
///     my_array: [2, 4, 6, 8],
/// };
/// let string = rudano::to_string_pretty(&value).unwrap();
///
/// let expected = r#"MyStruct {
///     my_array: [2, 4, 6, 8],
/// }"#;
///
/// assert_eq!(string, expected);
/// ```
pub fn serialize_array<S: Serializer, T: Serialize>(
    array: &[T],
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    let mut seq = serializer.serialize_seq(Some(array.len()))?;
    for e in array {
        seq.serialize_element(e)?;
    }
    seq.end()
}
