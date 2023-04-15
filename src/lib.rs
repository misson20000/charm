#![allow(dead_code)]
#![allow(clippy::single_match)]
#![allow(clippy::new_without_default)]
#![feature(type_alias_impl_trait)]

extern crate futures;
extern crate tokio;
extern crate send_wrapper;
extern crate enum_dispatch;

pub mod datapath;
pub mod model;
pub mod logic;
pub mod util;

#[cfg(feature = "gtk")]
pub mod view;
