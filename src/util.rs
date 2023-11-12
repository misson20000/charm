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

pub enum PhiIterator<Item, I1: Iterator<Item = Item>, I2: Iterator<Item = Item>, I3: Iterator<Item = Item>, I4: Iterator<Item = Item>> {
    I1(I1),
    I2(I2),
    I3(I3),
    I4(I4),
}

impl<Item, I1: Iterator<Item = Item>, I2: Iterator<Item = Item>, I3: Iterator<Item = Item>, I4: Iterator<Item = Item>> Iterator for PhiIterator<Item, I1, I2, I3, I4> {
    type Item = Item;

    fn next(&mut self) -> Option<Item> {
        match self {
            Self::I1(i) => i.next(),
            Self::I2(i) => i.next(),
            Self::I3(i) => i.next(),
            Self::I4(i) => i.next(),
        }
    }
}

impl<Item, I1: DoubleEndedIterator<Item = Item>, I2: DoubleEndedIterator<Item = Item>, I3: DoubleEndedIterator<Item = Item>, I4: DoubleEndedIterator<Item = Item>> DoubleEndedIterator for PhiIterator<Item, I1, I2, I3, I4> {
    fn next_back(&mut self) -> Option<Item> {
        match self {
            Self::I1(i) => i.next_back(),
            Self::I2(i) => i.next_back(),
            Self::I3(i) => i.next_back(),
            Self::I4(i) => i.next_back(),
        }
    }
}
