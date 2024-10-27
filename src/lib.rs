#![allow(dead_code)]
#![allow(clippy::single_match)]
#![allow(clippy::new_without_default)]

extern crate futures;
extern crate tokio;
extern crate send_wrapper;
extern crate enum_dispatch;

pub mod util;

pub mod model;

#[cfg(feature = "gtk")]
pub mod view;

pub mod serialization;
