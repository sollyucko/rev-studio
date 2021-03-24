use std::ops::{Add, Rem};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct NumBits(u16); // up to 512

// If these boilerplate ops impls accumulate, use a macro from crates.io instead

impl Add for NumBits {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

impl Rem<&NumBits> for NumBits {
    type Output = Self;

    fn rem(self, other: &Self) -> Self {
        Self(self.0 % other.0)
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Statement {
    Mov(Dest, Expr),
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Expr {
    Undefined,
    Immediate(NumBits, u64),
    Reg(Reg),
    Ptr(NumBits, Box<Expr>),
    Trunc(NumBits, Box<Expr>),
    Zext(NumBits, Box<Expr>),
    Sext(NumBits, Box<Expr>),
    Concat(Box<Expr>, Box<Expr>),
    // The NumBits is the chunk size, e.g. 1 for a bit reversal or 8 for a standard-size-byte reversal
    Reverse(Box<Expr>, NumBits),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Add(NumBits, Box<Expr>, Box<Expr>),
    Sub(NumBits, Box<Expr>, Box<Expr>),
    Mul(NumBits, Box<Expr>, Box<Expr>),
    UnsignedDiv(NumBits, Box<Expr>, Box<Expr>),
    UnsignedMod(NumBits, Box<Expr>, Box<Expr>),
    TruncDiv(NumBits, Box<Expr>, Box<Expr>),
    TruncMod(NumBits, Box<Expr>, Box<Expr>),
    SignedOverflowAdd(NumBits, Box<Expr>, Box<Expr>),
    SignedOverflowSub(NumBits, Box<Expr>, Box<Expr>),
    SignedOverflowMul(NumBits, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Dest {
    Reg(Reg),
    Ptr(NumBits, Box<Expr>),
    Concat(Box<Dest>, Box<Dest>),
}

// TODO
#[derive(Debug)]
pub enum Reg {}

impl Reg {
    pub fn size(&self) -> NumBits {
        match *self {}
    }
}

impl Expr {
    pub fn size(&self) -> Option<NumBits> {
        match self {
            Self::Undefined => None,
            Self::Immediate(size, _)
            | Self::Ptr(size, _)
            | Self::Trunc(size, _)
            | Self::Zext(size, _)
            | Self::Sext(size, _)
            | Self::Add(size, _, _)
            | Self::Sub(size, _, _)
            | Self::Mul(size, _, _)
            | Self::UnsignedDiv(size, _, _)
            | Self::UnsignedMod(size, _, _)
            | Self::TruncDiv(size, _, _)
            | Self::TruncMod(size, _, _) => Some(*size),
            Self::Reg(reg) => Some(reg.size()),
            Self::Concat(lhs, rhs) => Option::zip(lhs.size(), rhs.size()).map(|(x, y)| x + y),
            Self::Reverse(expr, chunk_size) => expr.size().map(|size| {
                    assert_eq!(
                        size % chunk_size,
                        NumBits(0),
                        "invalid bit-reversal: size {:?} (of expression {:?}) is not a multiple of chunk size {:?}",
                        size,
                        expr,
                        chunk_size,
                    );
                    size
                }),
            Self::Not(expr) | Self::Neg(expr) => expr.size(),
            Self::And(lhs, rhs) | Self::Or(lhs, rhs) | Self::Xor(lhs, rhs) => {
                let (size_lhs, size_rhs) = (lhs.size(), rhs.size());
                assert_eq!(size_lhs, size_rhs, "invalid bitwise operation: LHS {:?} and RHS {:?} have different sizes", lhs, rhs);
                size_lhs
            },
            Self::SignedOverflowAdd(_, _, _) | Self::SignedOverflowSub(_, _, _) | Self::SignedOverflowMul(_, _, _) => Some(NumBits(1)),
        }
    }

    pub fn simplify(self) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
