pub mod v1;

use crate::model::document;

pub use bincode::error::EncodeError as SerializationError;

#[derive(Debug)]
pub enum DeserializationError {
    InvalidMagic,
    UnsupportedVersion(u16),
    BincodeError(bincode::error::DecodeError),
}

fn bincode_configuration() -> bincode::config::Configuration {
    /* compatibility with original charm format written using bincode 1.3 */
    bincode::config::legacy().with_variable_int_encoding()
}

pub fn serialize_project(document: &document::Document) -> Result<Vec<u8>, SerializationError> {
    let mut vec: Vec<u8> = "charm".bytes().collect(); // magic
    vec.push(1); // version number MSB
    vec.push(0); // version number LSB

    vec.extend(bincode::serde::encode_to_vec(v1::Document::from(document), bincode_configuration())?.into_iter());

    Ok(vec)
}

pub fn deserialize_project(bytes: &[u8]) -> Result<document::Document, DeserializationError> {
    if &bytes[0..5] != "charm".as_bytes() {
        return Err(DeserializationError::InvalidMagic);
    }

    if bytes[5..7] != [1, 0] {
        return Err(DeserializationError::UnsupportedVersion(bytes[5] as u16 | ((bytes[6] as u16) << 8)));
    }

    bincode::serde::decode_from_slice::<v1::Document, bincode::config::Configuration>(&bytes[7..], bincode_configuration())
        .map_err(DeserializationError::BincodeError)
        .map(|(doc, _)| doc.into())
}
