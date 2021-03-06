use std::str::FromStr;

#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Address {
    pub byte: u64,
    pub bit: u8
}

#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug)]
pub struct Size {
    pub bytes: u64,
    pub bits: u8
}

#[derive(Default, PartialEq, Eq, Copy, Clone, Debug)]
pub struct Extent {
    pub begin: Address,
    pub end: Address,
}

pub enum ExtentTripletEnding {
    AtUnbounded,
    AfterUnbounded(Address),
    AfterBounded(Address, Address)
}

pub struct ExtentTriplet {
    pub before: Option<Address>,
    pub at: Address,
    pub after: ExtentTripletEnding,
}

pub mod unit {
    use crate::addr::{Address, Size};

    pub static NULL: Address = Address { byte: 0, bit: 0 };
    pub static REAL_END: Address = Address { byte: u64::MAX, bit: 8 }; // TODO: change this back to END

    pub static ZERO: Size = Size { bytes: 0, bits: 0 };
    pub static BIT: Size = Size { bytes: 0, bits: 1 };
    pub static NYBBLE: Size = Size { bytes: 0, bits: 4 };
    pub static BYTE: Size = Size { bytes: 1, bits: 0 };
    pub static BYTE_NYBBLE: Size = Size { bytes: 1, bits: 4 };
    pub static QWORD: Size = Size { bytes: 8, bits: 0 };
    pub static REAL_MAX: Size = Size { bytes: u64::MAX, bits: 8 }; // TODO: change this back to MAX
}

pub enum AddressParseError {
    MissingBytes,
    MalformedBytes(std::num::ParseIntError),
    MalformedBits(std::num::ParseIntError),
    TooManyBits,
}

impl Address {
    fn normalize_signed(bytes: u64, bits: i64) -> Address {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits < 0 {
            nbytes-= 1;
            nbits+= 8;
        }
        Address::normalize_unsigned(nbytes, nbits as u64)
    }
    
    fn normalize_unsigned(bytes: u64, bits: u64) -> Address {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits >= 8 && nbytes < u64::MAX { // TODO: replace this with division
            nbytes+= 1;
            nbits-= 8;
        }
        if nbits > 8 {
            panic!("address arithmetic overflow");
        } else {
            Address {byte: nbytes, bit: nbits as u8}
        }
    }

    pub fn parse(string: &str) -> Result<Address, AddressParseError> {
        let mut i = string.splitn(2, ".");

        Ok(Address {
            byte: i.next().ok_or(AddressParseError::MissingBytes)
                .map(|s| s.trim_start_matches("0x"))
                .and_then(|s| u64::from_str_radix(s, 16).map_err(|e| AddressParseError::MalformedBytes(e)))?,
            bit: i.next().map(|s| u8::from_str(s)
                              .map_err(|e| AddressParseError::MalformedBits(e))
                              .and_then(|v| if v < 8 { Ok(v) } else { Err(AddressParseError::TooManyBits) }))
                .unwrap_or(Ok(0))?
        })
    }
    
    pub fn magnitude(&self) -> Size {
        Size { bytes: self.byte, bits: self.bit }
    }

    pub fn round_down(&self) -> Address {
        Address { byte: self.byte, bit: 0 }
    }

    pub fn round_up(&self) -> Address {
        if self.bit == 0 || self.bit == 8 {
            *self
        } else {
            Address { byte: self.byte + 1, bit: 0 }
        }
    }
}

impl Size {
    fn normalize_signed(bytes: u64, bits: i64) -> Size {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits < 0 {
            nbytes-= 1;
            nbits+= 8;
        }
        Size::normalize_unsigned(nbytes, nbits as u64)
    }
    
    fn normalize_unsigned(bytes: u64, bits: u64) -> Size {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits >= 8 && nbytes < u64::MAX {
            nbytes+= 1;
            nbits-= 8;
        }
        if nbits > 8 {
            panic!("size arithmetic overflow");
        } else {
            Size {bytes: nbytes, bits: nbits as u8}
        }
    }

    pub fn round_up(&self) -> Size {
        if self.bits == 0 {
            *self
        } else {
            Size { bytes: self.bytes + 1, bits: 0 }
        }
    }

    pub fn floor(&self) -> Size {
        Size { bytes: self.bytes, bits: 0 }
    }
}

impl Extent {
    pub fn between(begin: Address, end: Address) -> Extent {
        Extent { begin, end }
    }

    pub fn between_bidirectional(a: Address, b: Address) -> Extent {
        Extent {
            begin: std::cmp::min(a, b),
            end: std::cmp::max(a, b)
        }
    }
    pub fn unbounded(begin: Address) -> Extent {
        Extent { begin, end: unit::REAL_END }
    }

    pub fn length(&self) -> Size {
        self.end - self.begin
    }
    
    pub fn round_out(&self) -> (u64, u64) { /* (addr, size) */
        let rd = self.begin.round_down();

        (rd.byte, (self.end.round_up() - rd).bytes)
    }

    pub fn includes(&self, addr: Address) -> bool {
        addr >= self.begin && addr < self.end
    }

    pub fn intersection(&self, other: Extent) -> Option<Extent> {
        let begin = std::cmp::max(self.begin, other.begin);
        let end = std::cmp::min(self.end, other.end);

        if end > begin {
            Some(Self::between(begin, end))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::addr;
    
    #[test]
    fn address_arithmetic() {
        let a = addr::Address { byte: 0x40008000, bit: 1 };
        let b = addr::Address { byte: 0x40008002, bit: 0 };
        let s = addr::Size { bytes: 1, bits: 7 };
        let t = addr::Size { bytes: 1, bits: 2 };
        
        assert_eq!(b - a, addr::Size { bytes: 1, bits: 7 });
        assert_eq!(a + s, b);
        assert_eq!(s + t, addr::Size { bytes: 3, bits: 1 });
        assert_eq!((a + s) - b, addr::Size::default());
    }

    #[test]
    fn size_arithmetic() {
        let a = addr::Size {bytes: 256, bits: 0};

        assert_eq!(a * 2, addr::Size {bytes: 512, bits: 0});
        assert_eq!(addr::Size { bytes: 256, bits: 1 } * 2, addr::Size {bytes: 512, bits: 2});
        assert_eq!(addr::Size { bytes: 256, bits: 4 } * 2, addr::Size {bytes: 513, bits: 0});
        
        assert_eq!(a / addr::unit::BYTE, 256);
        assert_eq!(a / addr::unit::BIT, 256 * 8);
        assert_eq!(a / addr::Size { bytes: 1, bits: 1 }, 227);
        assert_eq!(addr::Size { bytes: 256, bits: 1 } / addr::Size { bytes: 0, bits: 3 }, 683);
        assert_eq!(addr::Size { bytes: 939, bits: 1 } / addr::Size { bytes: 1, bits: 3 }, 683);
    }
}

/* address traits */

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bit {
            0 => write!(f, "{:#018x}", self.byte),
            _ => write!(f, "{:#018x}.{}", self.byte, self.bit)
        }
    }
}

impl std::fmt::Debug for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bit {
            0 => write!(f, "addr({:#018x})", self.byte),
            _ => write!(f, "addr({:#018x}.{})", self.byte, self.bit)
        }
    }
}

impl std::convert::From<u64> for Address {
    fn from(byte: u64) -> Address {
        Address {byte, bit: 0}
    }
}

impl std::ops::Sub<Address> for Address {
    type Output = Size;

    fn sub(self, rhs: Address) -> Size {
        Size::normalize_signed(self.byte - rhs.byte, self.bit as i64 - rhs.bit as i64)
    }
}

impl std::ops::Add<u64> for Address {
    type Output = Address;

    fn add(self, rhs: u64) -> Address {
        if self.byte >= 1 && self.bit == 0 && u64::MAX - rhs == self.byte - 1 {
            unit::REAL_END
        } else {
            Address {byte: self.byte + rhs, bit: self.bit}
        }
    }
}

impl std::ops::Add<Size> for Address {
    type Output = Address;

    fn add(self, rhs: Size) -> Address {
        if self.byte >= 1 && self.bit == 0 && u64::MAX - rhs.bytes == self.byte - 1 && rhs.bits == 0 {
            unit::REAL_END
        } else {
            Address::normalize_unsigned(self.byte + rhs.bytes, self.bit as u64 + rhs.bits as u64)
        }
    }
}

impl std::ops::AddAssign<u64> for Address {
    fn add_assign(&mut self, rhs: u64) {
        if self.byte >= 1 && self.bit == 0 && u64::MAX - rhs == self.byte - 1 {
            *self = unit::REAL_END
        } else {
            self.byte+= rhs;
        }
    }
}

impl std::ops::AddAssign<Size> for Address {
    fn add_assign(&mut self, rhs: Size) {
        if self.byte >= 1 && self.bit == 0 && u64::MAX - rhs.bytes == self.byte - 1 && rhs.bits == 0 {
            *self = unit::REAL_END
        } else {
            *self = Address::normalize_unsigned(self.byte + rhs.bytes, self.bit as u64 + rhs.bits as u64);
        }
    }
}

impl std::ops::Sub<u64> for Address {
    type Output = Address;

    fn sub(self, rhs: u64) -> Address {
        if self == unit::REAL_END {
            Address::normalize_unsigned(self.byte - rhs, self.bit as u64)
        } else {
            Address {byte: self.byte - rhs, bit: self.bit}
        }
    }
}

impl std::ops::Sub<Size> for Address {
    type Output = Address;

    fn sub(self, rhs: Size) -> Address {
        Address::normalize_signed(self.byte - rhs.bytes, self.bit as i64 - rhs.bits as i64)
    }
}

impl std::ops::SubAssign<Size> for Address {
    fn sub_assign(&mut self, rhs: Size) {
        *self = Address::normalize_signed(self.byte - rhs.bytes, self.bit as i64 - rhs.bits as i64);
    }
}

/* size traits */

impl std::fmt::Display for Size {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bits {
            0 => write!(f, "+{:#016x}", self.bytes),
            _ => write!(f, "+{:#016x}.{}", self.bytes, self.bits)
        }
    }
}

impl std::convert::From<u64> for Size {
    fn from(bytes: u64) -> Size {
        Size {bytes, bits: 0}
    }
}

impl std::ops::Add<Size> for Size {
    type Output = Size;

    fn add(self, rhs: Size) -> Size {
        Size::normalize_unsigned(self.bytes + rhs.bytes, self.bits as u64 + rhs.bits as u64)
    }
}

impl std::ops::Sub<Size> for Size {
    type Output = Size;

    fn sub(self, rhs: Size) -> Size {
        Size::normalize_signed(self.bytes - rhs.bytes, self.bits as i64 - rhs.bits as i64)
    }
}

impl std::ops::SubAssign<Size> for Size {
    fn sub_assign(&mut self, rhs: Size) {
        *self = Size::normalize_signed(self.bytes - rhs.bytes, self.bits as i64 - rhs.bits as i64);
    }
}

impl std::ops::Mul<u64> for Size {
    type Output = Size;

    fn mul(self, rhs: u64) -> Size {
        Size::normalize_unsigned(self.bytes * rhs, self.bits as u64 * rhs)
    }
}

/// NOTE: Unsafe for very large fractional divisors!
impl std::ops::Div<Size> for Size {
    type Output = u64;

    fn div(self, divisor: Size) -> u64 {
        let mut dividend = self;
        let mut acc_quotient = 0;

        if divisor.bytes > 0 {
            while dividend.bytes >= (divisor.bytes + 1) {
                let quotient = dividend.bytes / (divisor.bytes + 1);
                dividend-= divisor * quotient;
                acc_quotient+= quotient;
            }
        }

        while dividend >= divisor {
            let quotient = (dividend.bytes * 8 + dividend.bits as u64) / (divisor.bytes * 8 + divisor.bits as u64);
            dividend-= divisor * quotient;
            acc_quotient+= quotient;
        }

        acc_quotient
    }
}

impl std::ops::Rem<Size> for Size {
    type Output = Size;

    fn rem(self, divisor: Size) -> Size {
        let mut dividend = self;

        if divisor.bytes > 0 {
            while dividend.bytes >= (divisor.bytes + 1) {
                let quotient = dividend.bytes / (divisor.bytes + 1);
                dividend-= divisor * quotient;
            }
        }

        while dividend >= divisor {
            let quotient = (dividend.bytes * 8 + dividend.bits as u64) / (divisor.bytes * 8 + divisor.bits as u64);
            dividend-= divisor * quotient;
        }

        dividend
    }
}
