use std::str::FromStr;

// TODO: create a real distinction between offsets and addresses, and make OffsetExtent a thing.

#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub struct Address {
    pub byte: u64,
    pub bit: u8
}

pub type Offset = Size;

#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub struct Size {
    pub bytes: u64,
    pub bits: u8
}

#[derive(Default, PartialEq, Eq, Copy, Clone, Hash)]
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
    use super::{Address, Size, Extent};

    pub const NULL: Address = Address { byte: 0, bit: 0 };
    pub const END: Address = Address { byte: u64::MAX, bit: 8 };

    pub const ZERO: Size = Size { bytes: 0, bits: 0 };
    pub const BIT: Size = Size { bytes: 0, bits: 1 };
    pub const NYBBLE: Size = Size { bytes: 0, bits: 4 };
    pub const BYTE: Size = Size { bytes: 1, bits: 0 };
    pub const BYTE_NYBBLE: Size = Size { bytes: 1, bits: 4 };
    pub const QWORD: Size = Size { bytes: 8, bits: 0 };
    pub const MAX: Size = Size { bytes: u64::MAX, bits: 8 };

    pub const EMPTY: Extent = Extent { begin: NULL, end: NULL };
    pub const UNBOUNDED: Extent = Extent { begin: NULL, end: END };
}

#[derive(Debug)]
pub enum AddressParseError {
    MissingBytes,
    MalformedBytes(std::num::ParseIntError),
    MalformedBits(std::num::ParseIntError),
    TooManyBits,
}

#[derive(Debug)]
pub enum ExtentParseError {
    MissingBegin,
    MissingEnd,
    MalformedBegin(AddressParseError),
    MalformedEnd(AddressParseError),
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

    /// Parses a string of the form "[0x]1234[.5]" into an address. Addresses
    /// are always assumed to be in hexadecimal, regardless of whether the "0x"
    /// prefix is included or not. A bit offset can optionally be specified. If
    /// unspecified, it is assumed to be zero.
    pub fn parse(string: &str) -> Result<Address, AddressParseError> {
        let mut i = string.splitn(2, '.');

        Ok(Address {
            byte: i.next().ok_or(AddressParseError::MissingBytes)
                .map(|s| s.trim_start_matches("0x"))
                .and_then(|s| u64::from_str_radix(s, 16).map_err(AddressParseError::MalformedBytes))?,
            bit: i.next().map(|s| u8::from_str(s)
                              .map_err(AddressParseError::MalformedBits)
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

    pub fn to_size(&self) -> Size {
        Size { bytes: self.byte, bits: self.bit }
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

    pub fn to_addr(&self) -> Address {
        Address { byte: self.bytes, bit: self.bits }
    }


    /// Parses a string of the form "[0x]1234[.5]" into a size. Unlike
    /// addresses, sizes are NOT always assumed to be in hexadecimal and will
    /// parse as decimal if the "0x" prefix is not included. A bit offset can
    /// optionally be specified. If unspecified, it is assumed to be zero.
    pub fn parse(string: &str) -> Result<Size, AddressParseError> {
        let mut i = string.splitn(2, '.');

        let bytes = i.next().ok_or(AddressParseError::MissingBytes)?;

        let bytes = match bytes.strip_prefix("0x") {
            Some(hex_byte) => u64::from_str_radix(hex_byte, 16).map_err(AddressParseError::MalformedBytes)?,
            None => u64::from_str_radix(bytes, 10).map_err(AddressParseError::MalformedBytes)?,
        };

        let bits = i.next()
            .map(|s| u8::from_str(s)
                 .map_err(AddressParseError::MalformedBits)
                 .and_then(|v| if v < 8 { Ok(v) } else { Err(AddressParseError::TooManyBits) }))
            .unwrap_or(Ok(0))?;
        
        Ok(Size {
            bytes,
            bits,
        })
    }
}

impl Extent {
    pub fn between<T: Into<Address>>(begin: T, end: T) -> Extent {
        Extent { begin: begin.into(), end: end.into() }
    }

    pub fn between_bidirectional(a: Address, b: Address) -> Extent {
        Extent {
            begin: std::cmp::min(a, b),
            end: std::cmp::max(a, b)
        }
    }

    pub fn sized(begin: Address, size: Size) -> Extent {
        Extent { begin, end: begin + size }
    }
    
    pub fn sized_u64(begin: u64, size: u64) -> Extent {
        let begin = Address::from(begin);
        Extent { begin, end: begin + size }
    }
    
    pub fn unbounded(begin: Address) -> Extent {
        Extent { begin, end: unit::END }
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

    /// Returns whether or not this extent contains the entirety of the other extent.
    pub fn contains(&self, other: Extent) -> bool {
        other.begin >= self.begin && other.end <= self.end
    }

    /// Returns the intersection of this extent and the other, if they overlap. If they abut or don't overlap, None is returned.
    pub fn intersection(&self, other: Extent) -> Option<Extent> {
        let begin = std::cmp::max(self.begin, other.begin);
        let end = std::cmp::min(self.end, other.end);

        if end > begin {
            Some(Self::between(begin, end))
        } else {
            None
        }
    }

    pub fn rebase(&self, base: Address) -> Extent {
        Extent { begin: base + self.begin.to_size(), end: base + self.end.to_size() }
    }

    pub fn debase(&self, base: Address) -> Extent {
        Extent { begin: base - self.begin.to_size(), end: base - self.end.to_size() }
    }

    /// Parses an extent of the form "<begin>:(<end>|+<size>)", such as
    /// "0x100:+0x10" or "0x100:110". If the plus sign is included, the part
    /// after the colon is interpreted as a size instead of an end
    /// address. Remember that addresses always parse as hex, but sizes only
    /// parse as hex if "0x" is included.
    pub fn parse(string: &str) -> Result<Extent, ExtentParseError> {
        let mut i = string.splitn(2, ':');

        let begin = i.next().ok_or(ExtentParseError::MissingBegin).and_then(|b| Address::parse(b).map_err(ExtentParseError::MalformedBegin))?;
        let end = i.next().ok_or(ExtentParseError::MissingEnd)?;

        Ok(match end.strip_prefix("+") {
            Some(stripped) => Self::sized(begin, Size::parse(stripped).map_err(ExtentParseError::MalformedEnd)?),
            None => Self::between(begin, Address::parse(end).map_err(ExtentParseError::MalformedEnd)?),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::model::addr;
    
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
            0 => write!(f, "addr({:#x})", self.byte),
            _ => write!(f, "addr({:#x}.{})", self.byte, self.bit)
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
            unit::END
        } else {
            Address {byte: self.byte + rhs, bit: self.bit}
        }
    }
}

impl std::ops::Add<Size> for Address {
    type Output = Address;

    fn add(self, rhs: Size) -> Address {
        if self.byte >= 1 && self.bit == 0 && u64::MAX - rhs.bytes == self.byte - 1 && rhs.bits == 0 {
            unit::END
        } else {
            Address::normalize_unsigned(self.byte + rhs.bytes, self.bit as u64 + rhs.bits as u64)
        }
    }
}

impl std::ops::AddAssign<u64> for Address {
    fn add_assign(&mut self, rhs: u64) {
        if self.byte >= 1 && self.bit == 0 && u64::MAX - rhs == self.byte - 1 {
            *self = unit::END
        } else {
            self.byte+= rhs;
        }
    }
}

impl std::ops::AddAssign<Size> for Address {
    fn add_assign(&mut self, rhs: Size) {
        if self.byte >= 1 && self.bit == 0 && u64::MAX - rhs.bytes == self.byte - 1 && rhs.bits == 0 {
            *self = unit::END
        } else {
            *self = Address::normalize_unsigned(self.byte + rhs.bytes, self.bit as u64 + rhs.bits as u64);
        }
    }
}

impl std::ops::Sub<u64> for Address {
    type Output = Address;

    fn sub(self, rhs: u64) -> Address {
        if self == unit::END {
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

impl std::fmt::Debug for Size {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bits {
            0 => write!(f, "size({:#x})", self.bytes),
            _ => write!(f, "size({:#x}.{})", self.bytes, self.bits)
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
// TODO: uhh, what do we do about dividing max size by one bit?
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

/* extent traits */

impl std::fmt::Debug for Extent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extent({:?} to {:?})", self.begin, self.end)
    }
}

pub mod fmt {
    #[derive(Debug, Clone, Copy)]
    pub struct CompactSize(pub super::Size);
    
    impl std::fmt::Display for CompactSize {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.0.bits {
                0 => write!(f, "{:#x}", self.0.bytes),
                _ => write!(f, "{:#x}.{}", self.0.bytes, self.0.bits)
            }
        }
    }
}
