/// The term "listing" is used to refer to the representation of a document that the user interacts with, which is a
/// list of lines containing tokens.

/// The window module is used to describe how the listing is accessed. A listing is a stream of lines, and it is often
/// difficult to exactly represent a specific location to seek to within a listing; if you were to ask for the line at
/// address 0x4000a000, but there was a break at that address, it cannot be determined whether you mean to retrieve the
/// blank line from the break header, the break header line, or the hex line at that address, but we want to see all of
/// them. Instead, a generator pattern is used. The FlexWindow struct represents a variable-sized window into the
/// listing. It can be expanded or contracted both upwards and downwards to reliably enumerate the listing, but it
/// cannot reliably be seeked to arbitrary locations.

/// The cursor module is used to describe how the cursor moves around in the listing and accepts edits to it.

pub mod token;
pub mod layout;
//pub mod cursor;
