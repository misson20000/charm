use std::fmt;
use std::sync;
use std::task;
use std::vec;

use std::fmt::Write;

pub fn nybble_to_hex(nyb: u8) -> char {
    (if nyb < 10 {
        '0' as u8 + nyb
    } else {
        'a' as u8 + (nyb - 10)
    }) as char
}

pub fn fmt_hex_vec(vec: &vec::Vec<u8>) -> Result<std::string::String, fmt::Error> {
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

/* lol https://github.com/Kimundi/owning-ref-rs/issues/27#issuecomment-285807894 */
pub struct Holder<T> {
    pub held: T
}

impl<T> std::ops::Deref for Holder<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.held
    }
}

impl<T> std::ops::DerefMut for Holder<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.held
    }
}

impl<T> Holder<T> {
    pub fn new(value: T) -> Holder<T> {
        Holder { held: value }
    }
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
