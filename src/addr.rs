use std::str::FromStr;

#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug)]
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
    pub addr: Address,
    pub size: Size
}

#[derive(Default, PartialEq, Eq, Copy, Clone, Debug)]
pub struct InfiniteExtent {
    pub addr: Address,
    pub size: Option<Size>
}

// for use with extents
pub struct Triplet<T> {
    pub before: Option<T>,
    pub at: T,
    pub after: Option<T>
}

pub mod unit {
    use crate::addr::{Address, Size};

    pub static NULL: Address = Address { byte: 0, bit: 0 };
    pub static END: Address = Address { byte: u64::MAX, bit: 7 };

    pub static ZERO: Size = Size { bytes: 0, bits: 0 };
    pub static BIT: Size = Size { bytes: 0, bits: 1 };
    pub static NYBBLE: Size = Size { bytes: 0, bits: 4 };
    pub static BYTE: Size = Size { bytes: 1, bits: 0 };
    pub static BYTE_NYBBLE: Size = Size { bytes: 1, bits: 4 };
}

pub fn bitshift_span(span: &mut [u8], shift: u8) {
    let mut acc:u16 = 0; 
    // right-shift the entire output
    for b in span.iter_mut().rev() {
        acc<<= 8;
        acc|= (*b as u16) << 8 >> shift;
        *b = (acc >> 8) as u8;
    }
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bit {
            0 => write!(f, "{:#018x}", self.byte),
            _ => write!(f, "{:#018x}.{}", self.byte, self.bit)
        }
    }
}

impl std::ops::Sub<Address> for Address {
    type Output = Size;

    fn sub(self, rhs: Address) -> Size {
        Size::normalize_signed(self.byte - rhs.byte, self.bit as i8 - rhs.bit as i8)
    }
}

impl std::ops::Add<Size> for Address {
    type Output = Address;

    fn add(self, rhs: Size) -> Address {
        Address::normalize_unsigned(self.byte + rhs.bytes, self.bit + rhs.bits)
    }
}

impl std::ops::Sub<Size> for Address {
    type Output = Address;

    fn sub(self, rhs: Size) -> Address {
        Address::normalize_signed(self.byte - rhs.bytes, self.bit as i8 - rhs.bits as i8)
    }
}

impl std::ops::Add<u64> for Address {
    type Output = Address;

    fn add(self, rhs: u64) -> Address {
        Address {byte: self.byte + rhs, bit: self.bit}
    }
}

impl std::ops::AddAssign<u64> for Address {
    fn add_assign(&mut self, rhs: u64) {
        self.byte+= rhs;
    }
}

impl std::ops::AddAssign<Size> for Address {
    fn add_assign(&mut self, rhs: Size) {
        *self = Address::normalize_unsigned(self.byte + rhs.bytes, self.bit + rhs.bits);
    }
}

impl std::ops::SubAssign<Size> for Address {
    fn sub_assign(&mut self, rhs: Size) {
        *self = Address::normalize_signed(self.byte - rhs.bytes, self.bit as i8 - rhs.bits as i8);
    }
}

impl std::ops::Sub<u64> for Address {
    type Output = Address;

    fn sub(self, rhs: u64) -> Address {
        Address {byte: self.byte - rhs, bit: self.bit}
    }
}

pub enum AddressParseError {
    MissingBytes,
    MalformedBytes(std::num::ParseIntError),
    MalformedBits(std::num::ParseIntError),
    TooManyBits,
}

impl Address {
    fn normalize_signed(bytes: u64, bits: i8) -> Address {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits < 0 {
            nbytes-= 1;
            nbits+= 8;
        }
        Address::normalize_unsigned(nbytes, nbits as u8)
    }
    
    fn normalize_unsigned(bytes: u64, bits: u8) -> Address {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits >= 8 {
            nbytes+= 1;
            nbits-= 8;
        }
        Address {byte: nbytes, bit: nbits}
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

    pub fn bounded_distance_to_end(&self, bound: Size) -> Size {
        if bound == unit::ZERO {
            unit::ZERO
        } else if *self == unit::NULL {
            bound
        } else {
            std::cmp::min(bound, (unit::END - *self) + unit::BIT)
        }
    }

    pub fn is_close_to_end(&self, bound: Size) -> bool {
        if bound == unit::ZERO {
            false
        } else {
            *self >= (unit::END - (bound - unit::BIT))
        }
    }
}

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
        Size::normalize_unsigned(self.bytes + rhs.bytes, self.bits + rhs.bits)
    }
}

impl std::ops::Sub<Size> for Size {
    type Output = Size;

    fn sub(self, rhs: Size) -> Size {
        Size::normalize_signed(self.bytes - rhs.bytes, self.bits as i8 - rhs.bits as i8)
    }
}

impl Size {
    fn normalize_signed(bytes: u64, bits: i8) -> Size {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits < 0 {
            nbytes-= 1;
            nbits+= 8;
        }
        Size::normalize_unsigned(nbytes, nbits as u8)
    }
    
    fn normalize_unsigned(bytes: u64, bits: u8) -> Size {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits >= 8 {
            nbytes+= 1;
            nbits-= 8;
        }
        Size {bytes: nbytes, bits: nbits}
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
    pub fn new(addr: Address, size: Size) -> Extent {
        Extent { addr, size }
    }

    pub fn between(begin: Address, end: Address) -> Extent {
        Extent { addr: begin, size: end - begin }
    }

    pub fn hits_end_of_space(&self) -> bool {
        self.addr.is_close_to_end(self.size)
    }
    
    pub fn end(&self) -> Address {
        self.addr + self.size
    }

    pub fn round(&self) -> (u64, u64) {
        (self.addr.byte, (self.size + Size {bytes: 0, bits: self.addr.bit}).round_up().bytes)
    }
    
    pub fn contains(&self, addr: Address) -> bool {
        addr >= self.addr && addr - self.addr < self.size
    }

    pub fn contains_size(&self, size: Size) -> bool {
        size < self.size
    }
}

impl InfiniteExtent {
    pub fn new(addr: Address, size: Size) -> InfiniteExtent {
        InfiniteExtent { addr, size: Some(size) }
    }

    pub fn between(begin: Address, end: Address) -> InfiniteExtent {
        InfiniteExtent { addr: begin, size: Some(end - begin) }
    }

    pub fn infinite(addr: Address) -> InfiniteExtent {
        InfiniteExtent { addr, size: None }
    }

    // be careful with this
    pub fn end(&self) -> Option<Address> {
        self.size.map(|sz| self.addr + sz)
    }

    pub fn contains(&self, addr: Address) -> bool {
        addr >= self.addr && match self.size {
            Some(sz) => addr < self.addr + sz,
            None => true
        }
    }

    pub fn contains_size(&self, size: Size) -> bool {
        match self.size {
            Some(sz) => size < sz,
            None => true
        }
    }
    
    pub fn clip_from_end(&self, size: Size) -> Option<Address> {
        match self.size {
            Some(s) if size > s => Some(self.addr),
            Some(s) => Some(self.addr + (s - size)),
            None if size > unit::ZERO => Some(unit::END - (size - unit::BIT)),
            None => None
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
    fn bitspans() {
        {
            let a = addr::Address { byte:  0x40008001, bit:  2 };
            let s = addr::Size    { bytes: 2,          bits: 7 };

            assert_eq!(addr::round_span(a, s), (0x40008001, 4));
        }
        {
            let a = addr::Address { byte:  0x40008001, bit: 0};
            let s = addr::Size::from(2);

            assert_eq!(addr::round_span(a, s), (0x40008001, 2));
        }

        let mut a: [u8;6] = [1, 1, 1, 1, 1, 1];
        addr::bitshift_span(&mut a, 1);
        assert_eq!(a, [0x80, 0x80, 0x80, 0x80, 0x80, 0]);
    }
}
