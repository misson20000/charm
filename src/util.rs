
pub fn nybble_to_hex(nyb: u8) -> char {
    (if nyb < 10 {
        '0' as u8 + nyb
    } else {
        'a' as u8 + (nyb - 10)
    }) as char
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
