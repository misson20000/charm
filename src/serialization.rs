pub mod v1;

use crate::model::document;

use bincode::Options;

pub use bincode::Error as SerializationError;

#[derive(Debug)]
pub enum DeserializationError {
    InvalidMagic,
    UnsupportedVersion(u16),
    BincodeError(bincode::Error),
}

fn bincode_options() -> impl bincode::Options {
    bincode::DefaultOptions::new()
}

pub fn serialize_project(document: &document::Document) -> Result<Vec<u8>, SerializationError> {
    let mut vec: Vec<u8> = "charm".bytes().collect(); // magic
    vec.push(1); // version number MSB
    vec.push(0); // version number LSB

    vec.extend(bincode_options().serialize(&v1::Document::from(document))?.into_iter());

    Ok(vec)
}

pub fn deserialize_project(bytes: &[u8]) -> Result<document::Document, DeserializationError> {
    if &bytes[0..5] != "charm".as_bytes() {
        return Err(DeserializationError::InvalidMagic);
    }

    if bytes[5..7] != [1, 0] {
        return Err(DeserializationError::UnsupportedVersion(bytes[5] as u16 | ((bytes[6] as u16) << 8)));
    }

    bincode_options().deserialize::<v1::Document>(&bytes[7..]).map_err(DeserializationError::BincodeError).map(v1::Document::into)
}
