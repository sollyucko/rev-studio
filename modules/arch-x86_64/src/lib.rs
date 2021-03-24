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
    Msr(Box<Expr>),
    Pmc(u32),
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
    Msr(Box<Expr>),
    Ptr(NumBits, Box<Expr>),
    Concat(Box<Dest>, Box<Dest>),
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
#[non_exhaustive]
pub enum Reg {
    AL, CL, DL, BL, SPL, BPL, SIL, DIL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B,
    AH, CH, DH, BH,
    AX, CX, DX, BX, SP, BP, SI, DI, R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,
    EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15,
    FLAGS, EFLAGS, RFLAGS, CF, PF, AF, ZF, SF, TF, IF, DF, OF, IOPL, NT, RF, VM, AC, VIF, VIP, ID,
    IP, EIP, RIP,
    ES, CS, SS, DS, FS, GS,
    ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7,
    FSW, TOP, C0, C1, C2, C3, FCW, FTW,
    FIP, FDP, FCS, FDS, FOP,
    FPR0, FPR1, FPR2, FPR3, FPR4, FPR5, FPR6, FPR7,
    MMX0, MMX1, MMX2, MMX3, MMX4, MMX5, MMX6, MMX7,
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
    XMM16, XMM17, XMM18, XMM19, XMM20, XMM21, XMM22, XMM23, XMM24, XMM25, XMM26, XMM27, XMM28, XMM29, XMM30, XMM31,
    YMM0, YMM1, YMM2, YMM3, YMM4, YMM5, YMM6, YMM7, YMM8, YMM9, YMM10, YMM11, YMM12, YMM13, YMM14, YMM15,
    YMM16, YMM17, YMM18, YMM19, YMM20, YMM21, YMM22, YMM23, YMM24, YMM25, YMM26, YMM27, YMM28, YMM29, YMM30, YMM31,
    ZMM0, ZMM1, ZMM2, ZMM3, ZMM4, ZMM5, ZMM6, ZMM7, ZMM8, ZMM9, ZMM10, ZMM11, ZMM12, ZMM13, ZMM14, ZMM15,
    ZMM16, ZMM17, ZMM18, ZMM19, ZMM20, ZMM21, ZMM22, ZMM23, ZMM24, ZMM25, ZMM26, ZMM27, ZMM28, ZMM29, ZMM30, ZMM31,
    K0, K1, K2, K3, K4, K5, K6, K7, MXCSR, MXCSR_MASK,
    MSW, CR0, CR1, CR2, CR3, CR4, CR5, CR6, CR7, CR8, CR9, CR10, CR11, CR12, CR13, CR14, CR15,
    DR0, DR1, DR2, DR3, DR4, DR5, DR6, DR7, DR8, DR9, DR10, DR11, DR12, DR13, DR14, DR15,
    TR0, TR1, TR2, TR3, TR4, TR5, TR6, TR7,
    TMM0, TMM1, TMM2, TMM3, TMM4, TMM5, TMM6, TMM7,
    GDTR, LDTR, IDTR, TR,
    BND0, BND1, BND2, BND3, BNDCFG, BNDSTATUS,
    SSP, PKRU, UIF,
    XINUSE, XMODIFIED, XRSTOR_INFO, XCOMP_BV,

    // note: MSRs and APIC registers not included
}

impl Reg {
    pub const SPB: Self = Self::SPL; pub const BPB: Self = Self::BPL; pub const SIB: Self = Self::SIL; pub const DIB: Self = Self::DIL;

    pub const ST: Self = Self::ST0;
    pub const TPR: Self = Self::CR8;
    pub const TASK: Self = Self::TR; // FIXME: is this correct?
    pub const XFEATURE_ENABLED_MASK: Self = Self::XCR0;

    pub const X87CONTROL: Self = Self::FCW;
    pub const X87STATUS: Self = Self::FSW;
    pub const X87TAG: Self = Self::FTW;
    pub const FPSW: Self = Self::FSW;

    pub const FP_DP: Self = Self::FDP;
    pub const FP_DS: Self = Self::FDS;
    pub const FP_IP: Self = Self::FIP;
    pub const FP_CS: Self = Self::FCS;
    pub const FP_OPC: Self = Self::FOP;

    pub const F0: Self = Self::ST0; pub const F1: Self = Self::ST1; pub const F2: Self = Self::ST2; pub const F3: Self = Self::ST3;
    pub const F4: Self = Self::ST4; pub const F5: Self = Self::ST5; pub const F6: Self = Self::ST6; pub const F7: Self = Self::ST7;

    pub const M0: Self = Self::MMX0; pub const M1: Self = Self::MMX1; pub const M2: Self = Self::MMX2; pub const M3: Self = Self::MMX3;
    pub const M4: Self = Self::MMX4; pub const M5: Self = Self::MMX5; pub const M6: Self = Self::MMX6; pub const M7: Self = Self::MMX7;

    pub const X0: Self = Self::XMM0; pub const X1: Self = Self::XMM1; pub const X2: Self = Self::XMM2; pub const X3: Self = Self::XMM3;
    pub const X4: Self = Self::XMM4; pub const X5: Self = Self::XMM5; pub const X6: Self = Self::XMM6; pub const X7: Self = Self::XMM7;
    pub const X8: Self = Self::XMM8; pub const X9: Self = Self::XMM9; pub const X10: Self = Self::XMM10; pub const X11: Self = Self::XMM11;
    pub const X12: Self = Self::XMM12; pub const X13: Self = Self::XMM13; pub const X14: Self = Self::XMM14; pub const X15: Self = Self::XMM15;
    pub const X16: Self = Self::XMM16; pub const X17: Self = Self::XMM17; pub const X18: Self = Self::XMM18; pub const X19: Self = Self::XMM19;
    pub const X20: Self = Self::XMM20; pub const X21: Self = Self::XMM21; pub const X22: Self = Self::XMM22; pub const X23: Self = Self::XMM23;
    pub const X24: Self = Self::XMM24; pub const X25: Self = Self::XMM25; pub const X26: Self = Self::XMM26; pub const X27: Self = Self::XMM27;
    pub const X28: Self = Self::XMM28; pub const X29: Self = Self::XMM29; pub const X30: Self = Self::XMM30; pub const X31: Self = Self::XMM31;

    pub const Y0: Self = Self::YMM0; pub const Y1: Self = Self::YMM1; pub const Y2: Self = Self::YMM2; pub const Y3: Self = Self::YMM3;
    pub const Y4: Self = Self::YMM4; pub const Y5: Self = Self::YMM5; pub const Y6: Self = Self::YMM6; pub const Y7: Self = Self::YMM7;
    pub const Y8: Self = Self::YMM8; pub const Y9: Self = Self::YMM9; pub const Y10: Self = Self::YMM10; pub const Y11: Self = Self::YMM11;
    pub const Y12: Self = Self::YMM12; pub const Y13: Self = Self::YMM13; pub const Y14: Self = Self::YMM14; pub const Y15: Self = Self::YMM15;
    pub const Y16: Self = Self::YMM16; pub const Y17: Self = Self::YMM17; pub const Y18: Self = Self::YMM18; pub const Y19: Self = Self::YMM19;
    pub const Y20: Self = Self::YMM20; pub const Y21: Self = Self::YMM21; pub const Y22: Self = Self::YMM22; pub const Y23: Self = Self::YMM23;
    pub const Y24: Self = Self::YMM24; pub const Y25: Self = Self::YMM25; pub const Y26: Self = Self::YMM26; pub const Y27: Self = Self::YMM27;
    pub const Y28: Self = Self::YMM28; pub const Y29: Self = Self::YMM29; pub const Y30: Self = Self::YMM30; pub const Y31: Self = Self::YMM31;

    pub const Z0: Self = Self::ZMM0; pub const Z1: Self = Self::ZMM1; pub const Z2: Self = Self::ZMM2; pub const Z3: Self = Self::ZMM3;
    pub const Z4: Self = Self::ZMM4; pub const Z5: Self = Self::ZMM5; pub const Z6: Self = Self::ZMM6; pub const Z7: Self = Self::ZMM7;
    pub const Z8: Self = Self::ZMM8; pub const Z9: Self = Self::ZMM9; pub const Z10: Self = Self::ZMM10; pub const Z11: Self = Self::ZMM11;
    pub const Z12: Self = Self::ZMM12; pub const Z13: Self = Self::ZMM13; pub const Z14: Self = Self::ZMM14; pub const Z15: Self = Self::ZMM15;
    pub const Z16: Self = Self::ZMM16; pub const Z17: Self = Self::ZMM17; pub const Z18: Self = Self::ZMM18; pub const Z19: Self = Self::ZMM19;
    pub const Z20: Self = Self::ZMM20; pub const Z21: Self = Self::ZMM21; pub const Z22: Self = Self::ZMM22; pub const Z23: Self = Self::ZMM23;
    pub const Z24: Self = Self::ZMM24; pub const Z25: Self = Self::ZMM25; pub const Z26: Self = Self::ZMM26; pub const Z27: Self = Self::ZMM27;
    pub const Z28: Self = Self::ZMM28; pub const Z29: Self = Self::ZMM29; pub const Z30: Self = Self::ZMM30; pub const Z31: Self = Self::ZMM31;

    pub const R8L: Self = Self::R8B; pub const R9L: Self = Self::R9B; pub const R10L: Self = Self::R10B; pub const R11L: Self = Self::R11B;
    pub const R12L: Self = Self::R12B; pub const R13L: Self = Self::R13B; pub const R14L: Self = Self::R14B; pub const R15L: Self = Self::R15B;

    pub const DB0: Self = Self::DR0; pub const DB1: Self = Self::DR1; pub const DB2: Self = Self::DR2; pub const DB3: Self = Self::DR3;
    pub const DB4: Self = Self::DR4; pub const DB5: Self = Self::DR5; pub const DB6: Self = Self::DR6; pub const DB7: Self = Self::DR7;
    pub const DB8: Self = Self::DR8; pub const DB9: Self = Self::DR9; pub const DB10: Self = Self::DR10; pub const DB11: Self = Self::DR11;
    pub const DB12: Self = Self::DR12; pub const DB13: Self = Self::DR13; pub const DB14: Self = Self::DR14; pub const DB15: Self = Self::DR15;
}

/// Registers with the same RegSpace can potentially overlap
#[allow(non_camel_case_types)]
#[derive(Debug)]
#[non_exhaustive]
pub enum RegSpace {
    AX, CX, DX, BX, SP, BP, SI, DI, R8, R9, R10, R11, R12, R13, R14, R15,
    FLAGS, IP,
    ES, CS, SS, DS, FS, GS,
    FPR, FSW, FCW, FTW,
    FIP, FDP, FCS, FDS, FOP,
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
    XMM16, XMM17, XMM18, XMM19, XMM20, XMM21, XMM22, XMM23, XMM24, XMM25, XMM26, XMM27, XMM28, XMM29, XMM30, XMM31,
    K0, K1, K2, K3, K4, K5, K6, K7, MXCSR, MXCSR_MASK,
    MSW, CR0, CR1, CR2, CR3, CR4, CR5, CR6, CR7, CR8, CR9, CR10, CR11, CR12, CR13, CR14, CR15,
    DR0, DR1, DR2, DR3, DR4, DR5, DR6, DR7, DR8, DR9, DR10, DR11, DR12, DR13, DR14, DR15,
    TR0, TR1, TR2, TR3, TR4, TR5, TR6, TR7,
    TMM0, TMM1, TMM2, TMM3, TMM4, TMM5, TMM6, TMM7,
    GDTR, LDTR, IDTR, TR,
    BND0, BND1, BND2, BND3, BNDCFG, BNDSTATUS,
    SSP, PKRU, UIF,
    XINUSE, XMODIFIED, XRSTOR_INFO, XCOMP_BV,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum RegType {
    U8, U16, U32, U64, Bitfield, Flag, PrivLvl, Ptr16, Ptr32, Ptr64, Seg, Float, FloatStackTop, FpuOp, V128, V256, V512, VecMask, Tile, TableReg, Bounds,
}

pub enum CpuMode {
    Real,
    Unreal, // Real mode with 32-bit selectors
    SystemManagement, // 32-bit
    Virtual8086_16,
    Virtual8086_32,
    Protected16,
    Protected32,
    Long16,
    Long32,
    Long64,
}


impl Reg {
    pub fn size(&self) -> NumBits {
        todo!()
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
