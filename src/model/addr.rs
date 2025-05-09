use std::str::FromStr;

#[derive(Copy, Clone)]
pub struct AbsoluteAddressKind;
#[derive(Copy, Clone)]
pub struct OffsetKind;

pub trait Arithmetic {
}

impl Arithmetic for OffsetKind {
}

pub type AbsoluteAddress = Address<AbsoluteAddressKind>;
pub type Offset = Address<OffsetKind>;

pub struct Address<Kind> {
    bytes: u64,
    bits: u8,
    marker: std::marker::PhantomData<Kind>
}

impl<Kind> Address<Kind> {
    pub const fn new(bytes: u64, bits: u8) -> Self {
        Self {
            bytes,
            bits,
            marker: std::marker::PhantomData,
        }
    }

    fn force_from<OtherKind>(other: Address<OtherKind>) -> Self {
        Self::new(other.bytes, other.bits)
    }
    
    pub fn bytes(&self) -> u64 {
        self.bytes
    }
    
    pub fn bits(&self) -> u8 {
        self.bits
    }

    pub fn as_bits(&self) -> u64 {
        self.bytes * 8 + self.bits as u64
    }

    pub const fn zero() -> Self {
        Self::new(0, 0)
    }

    pub const fn infinity() -> Self {
        Self::new(u64::MAX, 8)
    }
    
    fn normalize_signed(bytes: u64, bits: i64) -> Self {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits < 0 {
            nbytes-= 1;
            nbits+= 8;
        }
        Self::normalize_unsigned(nbytes, nbits as u64)
    }
    
    fn normalize_unsigned(bytes: u64, bits: u64) -> Self {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits >= 8 && nbytes < u64::MAX { // TODO: replace this with division
            nbytes+= 1;
            nbits-= 8;
        }
        if nbits > 8 {
            panic!("address arithmetic overflow");
        } else {
            Self::new(nbytes, nbits as u8)
        }
    }

    fn normalize_unsigned_checked(bytes: u64, bits: u64) -> Option<Self> {
        let mut nbytes = bytes;
        let mut nbits = bits;
        while nbits >= 8 && nbytes < u64::MAX { // TODO: replace this with division
            nbytes = nbytes.checked_add(1)?;
            nbits-= 8;
        }
        if nbits > 8 {
            panic!("address arithmetic overflow");
        } else {
            Some(Self::new(nbytes, nbits as u8))
        }
    }

    /// Parses a string of the form "\[0x\]1234\[.5\]" into an
    /// address. A bit offset can optionally be specified. If
    /// unspecified, it is assumed to be zero.
    pub fn parse(string: &str, assume_hex: bool) -> Result<Self, AddressParseError> {
        let mut i = string.splitn(2, '.');

        let mut radix = 10;

        if assume_hex {
            radix = 16;
        }
        
        let mut byte_portion = i.next().ok_or(AddressParseError::EmptyString)?;

        if let Some(bp) = byte_portion.strip_prefix("0x") {
            radix = 16;
            byte_portion = bp;
        }
        
        let bytes = u64::from_str_radix(byte_portion, radix).map_err(AddressParseError::MalformedBytes)?;

        let bits = match i.next() {
            Some(bit_fragment) => u8::from_str(bit_fragment).map_err(AddressParseError::MalformedBits)?,
            None => 0
        };

        let bits = match bits {
            0..=7 => bits,
            /* Allow 8 to be specified iff we're parsing the special END address */
            8 if bytes == u64::MAX => 8,
            _ => return Err(AddressParseError::TooManyBits),
        };
                
        Ok(Self::new(bytes, bits))
    }

    pub fn round_down(&self) -> Self {
        Self::new(self.bytes(), 0)
    }

    pub fn round_up(&self) -> Self {
        if self.bits() == 0 || self.bits() == 8 {
            *self
        } else {
            Self::new(self.bytes() + 1, 0)
        }
    }

    fn add(self, rhs: Address<impl Arithmetic>) -> Self {
        Self::normalize_unsigned(self.bytes() + rhs.bytes(), self.bits() as u64 + rhs.bits() as u64)
    }
    
    pub fn checked_add(self, rhs: Address<impl Arithmetic>) -> Option<Self> {
        Self::normalize_unsigned_checked(self.bytes().checked_add(rhs.bytes())?, self.bits() as u64 + rhs.bits() as u64)
    }
    
    fn add_assign(&mut self, rhs: Address<impl Arithmetic>) {
        *self = Self::normalize_unsigned(self.bytes() + rhs.bytes(), self.bits() as u64 + rhs.bits() as u64);
    }

    fn sub<Other>(self, rhs: Address<Other>) -> Self {
        Self::normalize_signed(self.bytes() - rhs.bytes(), self.bits() as i64 - rhs.bits() as i64)
    }

    fn sub_assign(&mut self, rhs: Address<impl Arithmetic>) {
        *self = Self::normalize_signed(self.bytes() - rhs.bytes(), self.bits() as i64 - rhs.bits() as i64);
    }

    pub const NULL: Self = Self { bytes: 0, bits: 0, marker: std::marker::PhantomData };
    pub const ZERO: Self = Self { bytes: 0, bits: 0, marker: std::marker::PhantomData };
    pub const BIT: Self = Self { bytes: 0, bits: 1, marker: std::marker::PhantomData };
    pub const NYBBLE: Self = Self { bytes: 0, bits: 4, marker: std::marker::PhantomData };
    pub const BYTE: Self = Self { bytes: 1, bits: 0, marker: std::marker::PhantomData };
    pub const BYTE_NYBBLE: Self = Self { bytes: 1, bits: 4, marker: std::marker::PhantomData };
    pub const QWORD: Self = Self { bytes: 8, bits: 0, marker: std::marker::PhantomData };
    pub const MAX: Self = Self { bytes: u64::MAX, bits: 8, marker: std::marker::PhantomData };
}

pub struct Extent<Kind = OffsetKind> {
    pub begin: Address<Kind>,
    pub end: Address<Kind>,
}

pub enum ExtentTripletEnding<Kind> {
    AtUnbounded,
    AfterUnbounded(Address<Kind>),
    AfterBounded(Address<Kind>, Address<Kind>)
}

pub struct ExtentTriplet<Kind> {
    pub before: Option<Address<Kind>>,
    pub at: Address<Kind>,
    pub after: ExtentTripletEnding<Kind>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AddressParseError {
    EmptyString,
    MalformedBytes(std::num::ParseIntError),
    MalformedBits(std::num::ParseIntError),
    TooManyBits,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExtentParseError {
    MissingBegin,
    MissingEnd,
    MalformedBegin(AddressParseError),
    MalformedEnd(AddressParseError),
}

impl<Kind> Extent<Kind> where Address<Kind>: std::ops::Sub<Address<Kind>> {
    pub fn between<In: Into<Address<Kind>>>(begin: In, end: In) -> Self {
        let begin = begin.into();
        let end = end.into();
        assert!(begin <= end);
        Self { begin: begin.into(), end: end.into() }
    }

    pub fn between_bidirectional(a: Address<Kind>, b: Address<Kind>) -> Self {
        Self {
            begin: std::cmp::min(a, b),
            end: std::cmp::max(a, b)
        }
    }

    pub fn sized<T, U>(begin: T, size: U) -> Self where Address<Kind>: std::ops::AddAssign<U> + From<T> {
        let begin = begin.into();
        let mut end = begin;
        end+= size;
        Self { begin, end }
    }
    
    pub fn unbounded(begin: Address<Kind>) -> Self {
        Self { begin, end: Address::<Kind>::infinity() }
    }

    pub fn len(&self) -> <Address<Kind> as std::ops::Sub<Address<Kind>>>::Output {
        self.end - self.begin
    }

    pub fn is_empty(&self) -> bool {
        self.end == self.begin
    }
    
    pub fn round_out(&self) -> (u64, u64) { /* (addr, size) */
        let rd = self.begin.round_down();

        (rd.bytes(), self.end.round_up().sub(rd).bytes())
    }

    pub fn includes(&self, addr: Address<Kind>) -> bool {
        addr >= self.begin && addr < self.end
    }

    pub fn offset(&self, offset: Offset) -> Self {
        Self { begin: self.begin + offset, end: self.end + offset }
    }
    
    /// Returns whether or not this extent contains the entirety of the other extent.
    pub fn contains(&self, other: Self) -> bool {
        other.begin >= self.begin && other.end <= self.end
    }

    /// Returns the intersection of this extent and the other, if they overlap. If they abut or don't overlap, None is returned.
    pub fn intersection(&self, other: Self) -> Option<Self> {
        let begin = std::cmp::max(self.begin, other.begin);
        let end = std::cmp::min(self.end, other.end);

        if end > begin {
            Some(Self::between(begin, end))
        } else {
            None
        }
    }

    /// Parses an extent of the form "\<begin\>:(\<end\>|+\<size\>)",
    /// such as "0x100:+0x10" or "0x100:110". In the "begin:end" form,
    /// a 0x prefix applies to both the beginning and the end, but in
    /// the "begin:+size" form, the 0x prefix does not apply to the
    /// size.
    pub fn parse(mut string: &str, mut assume_hex: bool) -> Result<Self, ExtentParseError> {
        if let Some(s) = string.strip_prefix("0x") {
            assume_hex = true;
            string = s;
        }
        
        let mut i = string.splitn(2, ':');

        let begin = i.next().ok_or(ExtentParseError::MissingBegin).and_then(|b| Address::<Kind>::parse(b, assume_hex).map_err(ExtentParseError::MalformedBegin))?;
        let end = i.next().ok_or(ExtentParseError::MissingEnd)?;

        Ok(match end.strip_prefix("+") {
            Some(stripped) => Self::sized(begin, Offset::parse(stripped, false).map_err(ExtentParseError::MalformedEnd)?),
            None => Self::between(begin, Address::<Kind>::parse(end, assume_hex).map_err(ExtentParseError::MalformedEnd)?),
        })
    }
}

pub type AbsoluteExtent = Extent<AbsoluteAddressKind>;

impl<Kind> PartialEq for Extent<Kind> {
    fn eq(&self, rhs: &Self) -> bool {
        self.begin == rhs.begin && self.end == rhs.end
    }
}

impl Extent<AbsoluteAddressKind> {
    pub fn relative_to(&self, base: AbsoluteAddress) -> Extent<OffsetKind> {
        Extent { begin: base - self.begin, end: base - self.end }
    }
}

impl Extent<OffsetKind> {
    pub fn absolute_from(&self, base: AbsoluteAddress) -> Extent<AbsoluteAddressKind> {
        Extent { begin: base + self.begin, end: base + self.end }
    }
}

impl<Kind> Eq for Extent<Kind> {
}

impl<Kind> Copy for Extent<Kind> {
}

impl<Kind> Clone for Extent<Kind> {
    fn clone(&self) -> Self {
        Self {
            begin: self.begin,
            end: self.end,
        }
    }
}

impl<Kind> Default for Extent<Kind> {
    fn default() -> Self {
        Self {
            begin: Default::default(),
            end: Default::default(),
        }
    }
}

impl<Kind> std::hash::Hash for Extent<Kind> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.begin.hash(state);
        self.end.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use assert_matches::assert_matches;
    
    #[test]
    fn address_arithmetic() {
        let a = AbsoluteAddress::new(0x40008000, 1);
        let b = AbsoluteAddress::new(0x40008002, 0);
        let s = Offset::new(1, 7);
        let t = Offset::new(1, 2);
        
        assert_eq!(b - a, Offset::new(1, 7));
        assert_eq!(a + s, b);
        assert_eq!(s + t, Offset::new(3, 1));
        assert_eq!((a + s) - b, Offset::ZERO);
    }

    #[test]
    fn size_arithmetic() {
        let a = Offset::new(256, 0);

        assert_eq!(a * 2, Offset::new(512, 0));
        assert_eq!(Offset::new(256, 1) * 2, Offset::new(512, 2));
        assert_eq!(Offset::new(256, 4) * 2, Offset::new(513, 0));
        
        assert_eq!(a / Offset::BYTE, 256);
        assert_eq!(a / Offset::BIT, 256 * 8);
        assert_eq!(a / Offset::new(1, 1), 227);
        assert_eq!(Offset::new(256, 1) / Offset::new(0, 3), 683);
        assert_eq!(Offset::new(939, 1) / Offset::new(1, 3), 683);
    }

    #[test]
    fn address_parse() {
        assert_eq!(AbsoluteAddress::parse("123.4", true), Ok(AbsoluteAddress::new(0x123, 4)));
        assert_eq!(AbsoluteAddress::parse("0x123.4", false), Ok(AbsoluteAddress::new(0x123, 4)));
        assert_matches!(AbsoluteAddress::parse("blabla", true), Err(AddressParseError::MalformedBytes(_)));
        assert_matches!(AbsoluteAddress::parse("123.c", true), Err(AddressParseError::MalformedBits(_)));
        assert_eq!(AbsoluteAddress::parse("123.8", true), Err(AddressParseError::TooManyBits));
        assert_eq!(AbsoluteAddress::parse("ffffffffffffffff.8", true), Ok(AbsoluteAddress::infinity()));
        assert_eq!(AbsoluteAddress::parse("ffffffffffffffff.9", true), Err(AddressParseError::TooManyBits));
    }

    #[test]
    fn extent_comparisons() {
        assert_eq!(Extent::<OffsetKind>::sized(4, 5), Extent::<OffsetKind>::between(4, 9));
    }
}

/* address traits */

impl<Kind> std::convert::From<u64> for Address<Kind> {
    fn from(bytes: u64) -> Self {
        Self::new(bytes, 0)
    }
}

impl<Kind> PartialOrd for Address<Kind> {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl<Kind> Ord for Address<Kind> {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        self.bytes.cmp(&rhs.bytes).then(self.bits.cmp(&rhs.bits))
    }
}

impl<Kind> PartialEq for Address<Kind> {
    fn eq(&self, rhs: &Self) -> bool {
        self.bytes == rhs.bytes && self.bits == rhs.bits
    }
}

impl<Kind> Eq for Address<Kind> {
}

impl<Kind> Copy for Address<Kind> {
}

impl<Kind> Clone for Address<Kind> {
    fn clone(&self) -> Self {
        Self::new(self.bytes, self.bits)
    }
}

impl<Kind> Default for Address<Kind> {
    fn default() -> Self {
        Self::zero()
    }
}

impl<Kind> std::hash::Hash for Address<Kind> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.bytes.hash(state);
        self.bits.hash(state);
    }
}

impl<Kind1, Kind2: Arithmetic> std::ops::Add<Address<Kind2>> for Address<Kind1> {
    type Output = Self;
    fn add(self, rhs: Address<Kind2>) -> Self { Address::add(self, rhs) }
}

impl<Kind1, Kind2: Arithmetic> std::ops::AddAssign<Address<Kind2>> for Address<Kind1> {
    fn add_assign(&mut self, rhs: Address<Kind2>) { Address::add_assign(self, rhs) }
}

impl<Kind> std::ops::Add<u64> for Address<Kind> {
    type Output = Self;
    fn add(self, rhs: u64) -> Self { Address::new(self.bytes + rhs, self.bits) }
}

impl<Kind> std::ops::AddAssign<u64> for Address<Kind> {
    fn add_assign(&mut self, rhs: u64) {
        self.bytes+= rhs;
    }
}

impl std::ops::Sub<AbsoluteAddress> for AbsoluteAddress {
    type Output = Offset;
    fn sub(self, rhs: AbsoluteAddress) -> Offset { Offset::sub(Offset::force_from(self), Offset::force_from(rhs)) }
}

impl<Kind1, Kind2: Arithmetic> std::ops::Sub<Address<Kind2>> for Address<Kind1> {
    type Output = Self;
    fn sub(self, rhs: Address<Kind2>) -> Self { Address::sub(self, rhs) }
}

impl<Kind1, Kind2: Arithmetic> std::ops::SubAssign<Address<Kind2>> for Address<Kind1> {
    fn sub_assign(&mut self, rhs: Address<Kind2>) { Address::sub_assign(self, rhs) }
}

/* absolute address traits */

impl std::fmt::Display for AbsoluteAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bits {
            0 => write!(f, "{:#018x}", self.bytes),
            _ => write!(f, "{:#018x}.{}", self.bytes, self.bits)
        }
    }
}

impl std::fmt::Debug for AbsoluteAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bits {
            0 => write!(f, "addr({:#x})", self.bytes),
            _ => write!(f, "addr({:#x}.{})", self.bytes, self.bits)
        }
    }
}

/* offset traits */

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bits {
            0 => write!(f, "{:#x}", self.bytes),
            _ => write!(f, "{:#x}.{}", self.bytes, self.bits)
        }
    }
}

impl std::fmt::Debug for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bits {
            0 => write!(f, "offset({:#x})", self.bytes),
            _ => write!(f, "offset({:#x}.{})", self.bytes, self.bits)
        }
    }
}

impl std::ops::Mul<u64> for Offset {
    type Output = Offset;

    fn mul(self, rhs: u64) -> Offset {
        Offset::normalize_unsigned(self.bytes * rhs, self.bits as u64 * rhs)
    }
}

/// NOTE: Unsafe for very large fractional divisors!
// TODO: uhh, what do we do about dividing max size by one bit?
impl std::ops::Div<Offset> for Offset {
    type Output = u64;

    fn div(self, divisor: Offset) -> u64 {
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

impl std::ops::Rem<Offset> for Offset {
    type Output = Offset;

    fn rem(self, divisor: Offset) -> Offset {
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

impl<Kind> std::fmt::Debug for Extent<Kind> where Address<Kind>: std::fmt::Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extent({:?} to {:?})", self.begin, self.end)
    }
}
