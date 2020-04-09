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

pub fn round_span(addr: Address, extent: Size) -> (u64, u64) {
    (addr.byte, (extent + Size {bytes: 0, bits: addr.bit}).round_up().bytes)
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

impl std::ops::Sub<u64> for Address {
    type Output = Address;

    fn sub(self, rhs: u64) -> Address {
        Address {byte: self.byte - rhs, bit: self.bit}
    }
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
