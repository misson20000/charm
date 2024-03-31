#![allow(dead_code)]
#![allow(clippy::single_match)]
#![allow(clippy::new_without_default)]

extern crate futures;
extern crate tokio;
extern crate send_wrapper;
extern crate enum_dispatch;
#[macro_use]
extern crate rental;

pub mod util;

pub mod model;
pub mod logic;

#[cfg(feature = "gtk")]
pub mod view;
