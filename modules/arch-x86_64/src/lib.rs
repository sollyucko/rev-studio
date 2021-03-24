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
    ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7,
    FPR0, FPR1, FPR2, FPR3, FPR4, FPR5, FPR6, FPR7,
    MMX0, MMX1, MMX2, MMX3, MMX4, MMX5, MMX6, MMX7,
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
    XMM16, XMM17, XMM18, XMM19, XMM20, XMM21, XMM22, XMM23, XMM24, XMM25, XMM26, XMM27, XMM28, XMM29, XMM30, XMM31,
    YMM0, YMM1, YMM2, YMM3, YMM4, YMM5, YMM6, YMM7, YMM8, YMM9, YMM10, YMM11, YMM12, YMM13, YMM14, YMM15,
    YMM16, YMM17, YMM18, YMM19, YMM20, YMM21, YMM22, YMM23, YMM24, YMM25, YMM26, YMM27, YMM28, YMM29, YMM30, YMM31,
    ZMM0, ZMM1, ZMM2, ZMM3, ZMM4, ZMM5, ZMM6, ZMM7, ZMM8, ZMM9, ZMM10, ZMM11, ZMM12, ZMM13, ZMM14, ZMM15,
    ZMM16, ZMM17, ZMM18, ZMM19, ZMM20, ZMM21, ZMM22, ZMM23, ZMM24, ZMM25, ZMM26, ZMM27, ZMM28, ZMM29, ZMM30, ZMM31,
    ES, CS, SS, DS, FS, GS,
    CR0, CR1, CR2, CR3, CR4, CR5, CR6, CR7, CR8, CR9, CR10, CR11, CR12, CR13, CR14, CR15,
    DR0, DR1, DR2, DR3, DR4, DR5, DR6, DR7, DR8, DR9, DR10, DR11, DR12, DR13, DR14, DR15,
    FLAGS, EFLAGS, RFLAGS, CF, PF, AF, ZF, SF, TF, IF, DF, OF, IOPL, NT, RF, VM, AC, VIF, VIP, ID,
    IP, EIP, RIP,
    K0, K1, K2, K3, K4, K5, K6, K7,
    BND0, BND1, BND2, BND3,
    TR0, TR1, TR2, TR3, TR4, TR5, TR6, TR7,
    TMM0, TMM1, TMM2, TMM3, TMM4, TMM5, TMM6, TMM7,
    FSW, TOP, C0, C1, C2, C3, FCW, FTW,
    FP_IP, FP_DP, FP_CS, FP_DS, FP_OPC,

    // MSRs
    EFER, SYSCFG, STAR, LSTAR, CSTAR, SFMASK, FS_BASE, GS_BASE, KERNEL_GS_BASE, SYSENTER_CS, SYSENTER_ESP, SYSENTER_EIP,
    DEBUG_CTL, LAST_BRANCH_FROM_IP, LAST_BRANCH_TO_IP, LAST_INT_FROM_IP, LAST_INT_TO_IP,
    MTRR_CAP, MTRR_DEF_TYPE, PAT, TOP_MEM, TOP_MEM2, TSC, MCG_CAP, MCG_STATUS, MCG_CTL,
    MTRR_PHYS_BASE_0, MTRR_PHYS_BASE_1, MTRR_PHYS_BASE_2, MTRR_PHYS_BASE_3, MTRR_PHYS_BASE_4, MTRR_PHYS_BASE_5, MTRR_PHYS_BASE_6, MTRR_PHYS_BASE_7,
    MTRR_PHYS_MASK_0, MTRR_PHYS_MASK_1, MTRR_PHYS_MASK_2, MTRR_PHYS_MASK_3, MTRR_PHYS_MASK_4, MTRR_PHYS_MASK_5, MTRR_PHYS_MASK_6, MTRR_PHYS_MASK_7,
    MTRR_FIX_0, MTRR_FIX_1, MTRR_FIX_2, MTRR_FIX_3, MTRR_FIX_4, MTRR_FIX_5, MTRR_FIX_6, MTRR_FIX_7,
    PERF_EVT_SEL_0, PERF_EVT_SEL_1, PERF_EVT_SEL_2, PERF_EVT_SEL_3, PERF_EVT_SEL_4, PERF_EVT_SEL_5,
    PERF_CTR_0, PERF_CTR_1, PERF_CTR_2, PERF_CTR_3, PERF_CTR_4, PERF_CTR_5,
    MC0_CTL, MC1_CTL, MC2_CTL, MC3_CTL, MC4_CTL, MC5_CTL,
    MC0_STATUS, MC1_STATUS, MC2_STATUS, MC3_STATUS, MC4_STATUS, MC5_STATUS,
    MC0_ADDR, MC1_ADDR, MC2_ADDR, MC3_ADDR, MC4_ADDR, MC5_ADDR,
    MC0_MISC, MC1_MISC, MC2_MISC, MC3_MISC, MC4_MISC, MC5_MISC,
    PL0_SSP, PL1_SSP, PL2_SSP, PL3_SSP, ISST_ADDR, U_CET, S_CET, XSS,
    TSC_AUX, SMBASE,
    

    MSW, // Machine Status Word, low 16 bits of CR0
    MXCSR,
    GDTR, LDTR, IDTR, TR,
    EFER, // Extended features enable register.
    XCR0,
 
    BNDSCR, FTW, FDP, FDS, MXCSR_MASK, PKRU, XINUSE, XMODIFIED, XRSTOR_INFO, XCOMP_BV,
    PMC,
    FPSW,
    UIF, BNDCFG, BNDSTATUS, X87CONTROL, X87STATUS, X87TAG, TOP,
    SSP,
    F0, F1, F2, F3, F4, F5, F6, F7,
    TASK,
    
    // MSRs
    XSS, FS_BASE, GS_BASE, KERNEL_GS_BASE, LSTAR, SFMASK, STAR,
    IA32_RTIT_CTL, IA32_RTIT_OUTPUT_BASE, IA32_RTIT_OUTPUT_MASK_PTRS, IA32_RTIT_STATUS, IA32_RTIT_CR3_MATCH, IA32_RTIT_ADDR0_A, IA32_RTIT_ADDR0_B, IA32_RTIT_ADDR1_A, IA32_RTIT_ADDR1_B,

    // TODO?: PT, HDC
    // Not included: EIZ, RIZ
}

impl Reg {
    pub const ST: Self = Self::ST0;
    pub const TPR: Self = Self::CR8;
    pub const TASK: Self = Self::TR; // FIXME: is this correct?
    pub const XFEATURE_ENABLED_MASK: Self = Self::XCR0;

    pub const SPB: Self = Self::SPL;
    pub const BPB: Self = Self::BPL;
    pub const SIB: Self = Self::SIL;
    pub const DIB: Self = Self::DIL;

    // TODO: Fn => STn, Mn => MMXn, Xn => XMMn, RnL => RnB, DBn => DRn
}

pub enum RegFamily {
    AX, CX, DX, BX, SP, BP, SI, DI, R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,
    
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
