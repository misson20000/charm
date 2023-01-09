use std::fmt;
use std::sync;
use std::task;
use std::vec;

use std::fmt::Write;

pub fn nybble_to_hex(nyb: u8) -> char {
    (if nyb < 10 {
        b'0' + nyb
    } else {
        b'a' + (nyb - 10)
    }) as char
}

pub fn fmt_hex_slice(vec: &[u8]) -> Result<std::string::String, fmt::Error> {
    let mut acc = std::string::String::new();
    for (i, b) in vec.iter().enumerate() {
        if i % 8 == 0 {
            write!(&mut acc, "{:02x}  ", b)?;
        } else {
            write!(&mut acc, "{:02x} ", b)?;
        }
    }
    Ok(acc)
}

pub struct Notifier {
    wakers: sync::Mutex<vec::Vec<task::Waker>>,
}

impl Notifier {
    pub fn new() -> Notifier {
        Notifier {
            wakers: sync::Mutex::new(vec::Vec::new()),
        }
    }

    pub fn notify(&self) {
        for w in self.wakers.lock().unwrap().drain(..) {
            w.wake();
        }
    }

    pub fn enroll(&self, cx: &task::Context) {
        let waker = cx.waker();
        let mut wakers = self.wakers.lock().unwrap();
        if !wakers.iter().any(|w| w.will_wake(waker)) {
            wakers.push(waker.clone());
        }
    }
}

impl std::ops::Drop for Notifier {
    fn drop(&mut self) {
        self.notify();
    }
}

impl Default for Notifier {
    fn default() -> Notifier {
        Self::new()
    }
}
