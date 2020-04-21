use bitflags::bitflags;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;
use rev_studio_fmt_pyc::CodeObject;
use std::rc::Rc;

pub fn parse_code_obj(code_obj: Rc<CodeObject>) -> impl Iterator<Item = Result<Instruction, (Result<OpCode, u8>, u32)>> {
    instructions_from_op_codes(parse_extended_arg(op_codes_from_bytes(Rc::clone(&code_obj).iter_code_rc())), code_obj)
}

pub fn try_parse_code_obj(code_obj: Rc<CodeObject>) -> Result<Vec<Instruction>, (Result<OpCode, u8>, u32)> {
    parse_code_obj(code_obj).collect()
}

fn op_codes_from_bytes(
    code: impl Iterator<Item = u8> + Clone,
) -> impl Iterator<Item = (Result<OpCode, u8>, u8)> {
    let mut code2 = code.clone();
    code2.next();
    let op_codes = code.step_by(2).map(|x| OpCode::from_u8(x).ok_or(x));
    let args = code2.step_by(2);
    op_codes.zip(args)
}

struct ParseExtendedArgIter<CodeIter: Iterator<Item = (Result<OpCode, u8>, u8)>> {
    op_codes: CodeIter,
}

impl<CodeIter: Iterator<Item = (Result<OpCode, u8>, u8)>> Iterator
    for ParseExtendedArgIter<CodeIter>
{
    type Item = (Result<OpCode, u8>, u32);

    fn next(&mut self) -> Option<(Result<OpCode, u8>, u32)> {
        let mut value: u32 = 0;
        loop {
            let (op_code, byte) = self.op_codes.next()?;
            if op_code == Ok(OpCode::EXTENDED_ARG) {
                value *= 256;
                value += u32::from(byte);
            } else {
                return Some((op_code, value));
            }
        }
    }
}

fn parse_extended_arg(
    op_codes: impl Iterator<Item = (Result<OpCode, u8>, u8)>,
) -> impl Iterator<Item = (Result<OpCode, u8>, u32)> {
    ParseExtendedArgIter { op_codes }
}

// TODO: add past instructions
#[allow(non_camel_case_types)]
#[derive(PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum OpCode {
    POP_TOP = 1,
    ROT_TWO = 2,
    ROT_THREE = 3,
    DUP_TOP = 4,
    DUP_TOP_TWO = 5,
    ROT_FOUR = 6,
    NOP = 9,
    UNARY_POSITIVE = 10,
    UNARY_NEGATIVE = 11,
    UNARY_NOT = 12,
    UNARY_INVERT = 15,
    BINARY_MATRIX_MULTIPLY = 16,
    INPLACE_MATRIX_MULTIPLY = 17,
    BINARY_POWER = 19,
    BINARY_MULTIPLY = 20,
    BINARY_MODULO = 22,
    BINARY_ADD = 23,
    BINARY_SUBTRACT = 24,
    BINARY_SUBSCR = 25,
    BINARY_FLOOR_DIVIDE = 26,
    BINARY_TRUE_DIVIDE = 27,
    INPLACE_FLOOR_DIVIDE = 28,
    INPLACE_TRUE_DIVIDE = 29,
    RERAISE = 48,
    WITH_EXCEPT_START = 49,
    GET_AITER = 50,
    GET_ANEXT = 51,
    BEFORE_ASYNC_WITH = 52,
    END_ASYNC_FOR = 54,
    INPLACE_ADD = 55,
    INPLACE_SUBTRACT = 56,
    INPLACE_MULTIPLY = 57,
    INPLACE_MODULO = 59,
    STORE_SUBSCR = 60,
    DELETE_SUBSCR = 61,
    BINARY_LSHIFT = 62,
    BINARY_RSHIFT = 63,
    BINARY_AND = 64,
    BINARY_XOR = 65,
    BINARY_OR = 66,
    INPLACE_POWER = 67,
    GET_ITER = 68,
    GET_YIELD_FROM_ITER = 69,
    PRINT_EXPR = 70,
    LOAD_BUILD_CLASS = 71,
    YIELD_FROM = 72,
    GET_AWAITABLE = 73,
    LOAD_ASSERTION_ERROR = 74,
    INPLACE_LSHIFT = 75,
    INPLACE_RSHIFT = 76,
    INPLACE_AND = 77,
    INPLACE_XOR = 78,
    INPLACE_OR = 79,
    LIST_TO_TUPLE = 82,
    RETURN_VALUE = 83,
    IMPORT_STAR = 84,
    SETUP_ANNOTATIONS = 85,
    YIELD_VALUE = 86,
    POP_BLOCK = 87,
    POP_EXCEPT = 89,
    STORE_NAME = 90,
    DELETE_NAME = 91,
    UNPACK_SEQUENCE = 92,
    FOR_ITER = 93,
    UNPACK_EX = 94,
    STORE_ATTR = 95,
    DELETE_ATTR = 96,
    STORE_GLOBAL = 97,
    DELETE_GLOBAL = 98,
    LOAD_CONST = 100,
    LOAD_NAME = 101,
    BUILD_TUPLE = 102,
    BUILD_LIST = 103,
    BUILD_SET = 104,
    BUILD_MAP = 105,
    LOAD_ATTR = 106,
    COMPARE_OP = 107,
    IMPORT_NAME = 108,
    IMPORT_FROM = 109,
    JUMP_FORWARD = 110,
    JUMP_IF_FALSE_OR_POP = 111,
    JUMP_IF_TRUE_OR_POP = 112,
    JUMP_ABSOLUTE = 113,
    POP_JUMP_IF_FALSE = 114,
    POP_JUMP_IF_TRUE = 115,
    LOAD_GLOBAL = 116,
    IS_OP = 117,
    CONTAINS_OP = 118,
    JUMP_IF_NOT_EXC_MATCH = 121,
    SETUP_FINALLY = 122,
    LOAD_FAST = 124,
    STORE_FAST = 125,
    DELETE_FAST = 126,
    RAISE_VARARGS = 130,
    CALL_FUNCTION = 131,
    MAKE_FUNCTION = 132,
    BUILD_SLICE = 133,
    LOAD_CLOSURE = 135,
    LOAD_DEREF = 136,
    STORE_DEREF = 137,
    DELETE_DEREF = 138,
    CALL_FUNCTION_KW = 141,
    CALL_FUNCTION_EX = 142,
    SETUP_WITH = 143,
    EXTENDED_ARG = 144,
    LIST_APPEND = 145,
    SET_ADD = 146,
    MAP_ADD = 147,
    LOAD_CLASSDEREF = 148,
    SETUP_ASYNC_WITH = 154,
    FORMAT_VALUE = 155,
    BUILD_CONST_KEY_MAP = 156,
    BUILD_STRING = 157,
    LOAD_METHOD = 160,
    CALL_METHOD = 161,
    LIST_EXTEND = 162,
    SET_UPDATE = 163,
    DICT_MERGE = 164,
    DICT_UPDATE = 165,
}

#[derive(PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum CmpOp {
    Lt = 0, // <
    Le = 1, // <=
    Eq = 2, // ==
    Ne = 3, // !=
    Gt = 4, // >
    Ge = 5, // >=
    In = 6,
    NotIn = 7,
    Is = 8,
    IsNot = 9,
    ExceptionMatch = 10,
    Bad = 11,
}

#[derive(PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum RaiseForm {
    ReRaise = 0,
    Raise = 1,
    RaiseFrom = 2,
}

#[derive(PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum BuildSliceArgc {
    StartStop = 2,
    StartStopStep = 3,
}

bitflags! {
    #[derive(Default)]
    pub struct MakeFunctionFlags: u8 {
        const POSITIONAL_DEFAULTS = 0b0000_0001;
        const KWONLY_DEFAULTS     = 0b0000_0010;
        const ANNOTATIONS         = 0b0000_0100;
        const FREEVARS            = 0b0000_1000;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct CallFunctionExFlags: u8 {
        const KWARGS = 0b0000_0001;
    }
}

// TODO: may depend on version
#[allow(clippy::too_many_lines)]
fn instructions_from_op_codes(
    op_codes: impl Iterator<Item = (Result<OpCode, u8>, u32)>,
    code_obj: Rc<CodeObject>,
) -> impl Iterator<Item = Result<Instruction, (Result<OpCode, u8>, u32)>> {
    op_codes.map(move |(op_code, data)| {
        (|| match op_code {
            Ok(OpCode::POP_TOP) => Some(Instruction::POP_TOP),
            Ok(OpCode::ROT_TWO) => Some(Instruction::ROT_TWO),
            Ok(OpCode::ROT_THREE) => Some(Instruction::ROT_THREE),
            Ok(OpCode::DUP_TOP) => Some(Instruction::DUP_TOP),
            Ok(OpCode::DUP_TOP_TWO) => Some(Instruction::DUP_TOP_TWO),
            Ok(OpCode::ROT_FOUR) => Some(Instruction::ROT_FOUR),
            Ok(OpCode::NOP) => Some(Instruction::NOP),
            Ok(OpCode::UNARY_POSITIVE) => Some(Instruction::UNARY_POSITIVE),
            Ok(OpCode::UNARY_NEGATIVE) => Some(Instruction::UNARY_NEGATIVE),
            Ok(OpCode::UNARY_NOT) => Some(Instruction::UNARY_NOT),
            Ok(OpCode::UNARY_INVERT) => Some(Instruction::UNARY_INVERT),
            Ok(OpCode::BINARY_MATRIX_MULTIPLY) => Some(Instruction::BINARY_MATRIX_MULTIPLY),
            Ok(OpCode::INPLACE_MATRIX_MULTIPLY) => Some(Instruction::INPLACE_MATRIX_MULTIPLY),
            Ok(OpCode::BINARY_POWER) => Some(Instruction::BINARY_POWER),
            Ok(OpCode::BINARY_MULTIPLY) => Some(Instruction::BINARY_MULTIPLY),
            Ok(OpCode::BINARY_MODULO) => Some(Instruction::BINARY_MODULO),
            Ok(OpCode::BINARY_ADD) => Some(Instruction::BINARY_ADD),
            Ok(OpCode::BINARY_SUBTRACT) => Some(Instruction::BINARY_SUBTRACT),
            Ok(OpCode::BINARY_SUBSCR) => Some(Instruction::BINARY_SUBSCR),
            Ok(OpCode::BINARY_FLOOR_DIVIDE) => Some(Instruction::BINARY_FLOOR_DIVIDE),
            Ok(OpCode::BINARY_TRUE_DIVIDE) => Some(Instruction::BINARY_TRUE_DIVIDE),
            Ok(OpCode::INPLACE_FLOOR_DIVIDE) => Some(Instruction::INPLACE_FLOOR_DIVIDE),
            Ok(OpCode::INPLACE_TRUE_DIVIDE) => Some(Instruction::INPLACE_TRUE_DIVIDE),
            Ok(OpCode::RERAISE) => Some(Instruction::RERAISE),
            Ok(OpCode::WITH_EXCEPT_START) => Some(Instruction::WITH_EXCEPT_START),
            Ok(OpCode::GET_AITER) => Some(Instruction::GET_AITER),
            Ok(OpCode::GET_ANEXT) => Some(Instruction::GET_ANEXT),
            Ok(OpCode::BEFORE_ASYNC_WITH) => Some(Instruction::BEFORE_ASYNC_WITH),
            Ok(OpCode::END_ASYNC_FOR) => Some(Instruction::END_ASYNC_FOR),
            Ok(OpCode::INPLACE_ADD) => Some(Instruction::INPLACE_ADD),
            Ok(OpCode::INPLACE_SUBTRACT) => Some(Instruction::INPLACE_SUBTRACT),
            Ok(OpCode::INPLACE_MULTIPLY) => Some(Instruction::INPLACE_MULTIPLY),
            Ok(OpCode::INPLACE_MODULO) => Some(Instruction::INPLACE_MODULO),
            Ok(OpCode::STORE_SUBSCR) => Some(Instruction::STORE_SUBSCR),
            Ok(OpCode::DELETE_SUBSCR) => Some(Instruction::DELETE_SUBSCR),
            Ok(OpCode::BINARY_LSHIFT) => Some(Instruction::BINARY_LSHIFT),
            Ok(OpCode::BINARY_RSHIFT) => Some(Instruction::BINARY_RSHIFT),
            Ok(OpCode::BINARY_AND) => Some(Instruction::BINARY_AND),
            Ok(OpCode::BINARY_XOR) => Some(Instruction::BINARY_XOR),
            Ok(OpCode::BINARY_OR) => Some(Instruction::BINARY_OR),
            Ok(OpCode::INPLACE_POWER) => Some(Instruction::INPLACE_POWER),
            Ok(OpCode::GET_ITER) => Some(Instruction::GET_ITER),
            Ok(OpCode::GET_YIELD_FROM_ITER) => Some(Instruction::GET_YIELD_FROM_ITER),
            Ok(OpCode::PRINT_EXPR) => Some(Instruction::PRINT_EXPR),
            Ok(OpCode::LOAD_BUILD_CLASS) => Some(Instruction::LOAD_BUILD_CLASS),
            Ok(OpCode::YIELD_FROM) => Some(Instruction::YIELD_FROM),
            Ok(OpCode::GET_AWAITABLE) => Some(Instruction::GET_AWAITABLE),
            Ok(OpCode::LOAD_ASSERTION_ERROR) => Some(Instruction::LOAD_ASSERTION_ERROR),
            Ok(OpCode::INPLACE_LSHIFT) => Some(Instruction::INPLACE_LSHIFT),
            Ok(OpCode::INPLACE_RSHIFT) => Some(Instruction::INPLACE_RSHIFT),
            Ok(OpCode::INPLACE_AND) => Some(Instruction::INPLACE_AND),
            Ok(OpCode::INPLACE_XOR) => Some(Instruction::INPLACE_XOR),
            Ok(OpCode::INPLACE_OR) => Some(Instruction::INPLACE_OR),
            Ok(OpCode::LIST_TO_TUPLE) => Some(Instruction::LIST_TO_TUPLE),
            Ok(OpCode::RETURN_VALUE) => Some(Instruction::RETURN_VALUE),
            Ok(OpCode::IMPORT_STAR) => Some(Instruction::IMPORT_STAR),
            Ok(OpCode::SETUP_ANNOTATIONS) => Some(Instruction::SETUP_ANNOTATIONS),
            Ok(OpCode::YIELD_VALUE) => Some(Instruction::YIELD_VALUE),
            Ok(OpCode::POP_BLOCK) => Some(Instruction::POP_BLOCK),
            Ok(OpCode::POP_EXCEPT) => Some(Instruction::POP_EXCEPT),
            Ok(OpCode::STORE_NAME) => Some(Instruction::STORE_NAME(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::DELETE_NAME) => Some(Instruction::DELETE_NAME(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::UNPACK_SEQUENCE) => Some(Instruction::UNPACK_SEQUENCE(data)),
            Ok(OpCode::FOR_ITER) => Some(Instruction::FOR_ITER(data)),
            Ok(OpCode::UNPACK_EX) => {
                let [_, _, after, before] = data.to_be_bytes();
                Some(Instruction::UNPACK_EX { before, after })
            }
            Ok(OpCode::STORE_ATTR) => Some(Instruction::STORE_ATTR(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::DELETE_ATTR) => Some(Instruction::DELETE_ATTR(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::STORE_GLOBAL) => Some(Instruction::STORE_GLOBAL(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::DELETE_GLOBAL) => Some(Instruction::DELETE_GLOBAL(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::LOAD_CONST) => Some(Instruction::LOAD_CONST(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::LOAD_NAME) => Some(Instruction::LOAD_NAME(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::BUILD_TUPLE) => Some(Instruction::BUILD_TUPLE(data)),
            Ok(OpCode::BUILD_LIST) => Some(Instruction::BUILD_LIST(data)),
            Ok(OpCode::BUILD_SET) => Some(Instruction::BUILD_SET(data)),
            Ok(OpCode::BUILD_MAP) => Some(Instruction::BUILD_MAP(data)),
            Ok(OpCode::LOAD_ATTR) => Some(Instruction::LOAD_ATTR(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::COMPARE_OP) => Some(Instruction::COMPARE_OP(CmpOp::from_u32(data)?)),
            Ok(OpCode::IMPORT_NAME) => Some(Instruction::IMPORT_NAME(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::IMPORT_FROM) => Some(Instruction::IMPORT_FROM(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::JUMP_FORWARD) => Some(Instruction::JUMP_FORWARD(data)),
            Ok(OpCode::JUMP_IF_FALSE_OR_POP) => Some(Instruction::JUMP_IF_FALSE_OR_POP(data)),
            Ok(OpCode::JUMP_IF_TRUE_OR_POP) => Some(Instruction::JUMP_IF_TRUE_OR_POP(data)),
            Ok(OpCode::JUMP_ABSOLUTE) => Some(Instruction::JUMP_ABSOLUTE(data)),
            Ok(OpCode::POP_JUMP_IF_FALSE) => Some(Instruction::POP_JUMP_IF_FALSE(data)),
            Ok(OpCode::POP_JUMP_IF_TRUE) => Some(Instruction::POP_JUMP_IF_TRUE(data)),
            Ok(OpCode::LOAD_GLOBAL) => Some(Instruction::LOAD_GLOBAL(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::IS_OP) => Some(Instruction::IS_OP(data == 0)),
            Ok(OpCode::CONTAINS_OP) => Some(Instruction::CONTAINS_OP(data == 0)),
            Ok(OpCode::JUMP_IF_NOT_EXC_MATCH) => Some(Instruction::JUMP_IF_NOT_EXC_MATCH(data)),
            Ok(OpCode::SETUP_FINALLY) => Some(Instruction::SETUP_FINALLY(data)),
            Ok(OpCode::LOAD_FAST) => Some(Instruction::LOAD_FAST(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::STORE_FAST) => Some(Instruction::STORE_FAST(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::DELETE_FAST) => Some(Instruction::DELETE_FAST(
                code_obj.names[data as usize].clone(),
            )),
            Ok(OpCode::RAISE_VARARGS) => {
                Some(Instruction::RAISE_VARARGS(RaiseForm::from_u32(data)?))
            }
            Ok(OpCode::CALL_FUNCTION) => Some(Instruction::CALL_FUNCTION(data)),
            Ok(OpCode::MAKE_FUNCTION) => Some(Instruction::MAKE_FUNCTION(
                MakeFunctionFlags::from_bits(u8::from_u32(data)?)?,
            )),
            Ok(OpCode::BUILD_SLICE) => {
                Some(Instruction::BUILD_SLICE(BuildSliceArgc::from_u32(data)?))
            }
            Ok(OpCode::LOAD_CLOSURE) => Some(Instruction::LOAD_CLOSURE(data)),
            Ok(OpCode::LOAD_DEREF) => Some(Instruction::LOAD_DEREF(data)),
            Ok(OpCode::STORE_DEREF) => Some(Instruction::STORE_DEREF(data)),
            Ok(OpCode::DELETE_DEREF) => Some(Instruction::DELETE_DEREF(data)),
            Ok(OpCode::CALL_FUNCTION_KW) => Some(Instruction::CALL_FUNCTION_KW(data)),
            Ok(OpCode::CALL_FUNCTION_EX) => Some(Instruction::CALL_FUNCTION_EX(
                CallFunctionExFlags::from_bits(u8::from_u32(data)?)?,
            )),
            Ok(OpCode::SETUP_WITH) => Some(Instruction::SETUP_WITH(data)),
            Ok(OpCode::LIST_APPEND) => Some(Instruction::LIST_APPEND),
            Ok(OpCode::SET_ADD) => Some(Instruction::SET_ADD),
            Ok(OpCode::MAP_ADD) => Some(Instruction::MAP_ADD),
            Ok(OpCode::LOAD_CLASSDEREF) => Some(Instruction::LOAD_CLASSDEREF(data)),
            Ok(OpCode::SETUP_ASYNC_WITH) => Some(Instruction::SETUP_ASYNC_WITH),
            Ok(OpCode::FORMAT_VALUE) => Some(Instruction::FORMAT_VALUE),
            Ok(OpCode::BUILD_CONST_KEY_MAP) => Some(Instruction::BUILD_CONST_KEY_MAP(data)),
            Ok(OpCode::BUILD_STRING) => Some(Instruction::BUILD_STRING(data)),
            Ok(OpCode::LOAD_METHOD) => Some(Instruction::LOAD_METHOD),
            Ok(OpCode::CALL_METHOD) => Some(Instruction::CALL_METHOD),
            Ok(OpCode::LIST_EXTEND) => Some(Instruction::LIST_EXTEND(data)),
            Ok(OpCode::SET_UPDATE) => Some(Instruction::SET_UPDATE(data)),
            Ok(OpCode::DICT_MERGE) => Some(Instruction::DICT_MERGE(data)),
            Ok(OpCode::DICT_UPDATE) => Some(Instruction::DICT_UPDATE(data)),
            _ => None,
        })()
        .ok_or((op_code, data))
    })
}

/// The order is arbitrary and may change at any time
#[allow(non_camel_case_types)]
#[derive(PartialEq, Eq)]
pub enum Instruction {
    POP_TOP,
    ROT_TWO,
    ROT_THREE,
    DUP_TOP,
    DUP_TOP_TWO,
    ROT_FOUR,
    NOP,
    UNARY_POSITIVE,
    UNARY_NEGATIVE,
    UNARY_NOT,
    UNARY_INVERT,
    BINARY_MATRIX_MULTIPLY,
    INPLACE_MATRIX_MULTIPLY,
    BINARY_POWER,
    BINARY_MULTIPLY,
    BINARY_MODULO,
    BINARY_ADD,
    BINARY_SUBTRACT,
    BINARY_SUBSCR,
    BINARY_FLOOR_DIVIDE,
    BINARY_TRUE_DIVIDE,
    INPLACE_FLOOR_DIVIDE,
    INPLACE_TRUE_DIVIDE,
    RERAISE,
    WITH_EXCEPT_START,
    GET_AITER,
    GET_ANEXT,
    BEFORE_ASYNC_WITH,
    END_ASYNC_FOR,
    INPLACE_ADD,
    INPLACE_SUBTRACT,
    INPLACE_MULTIPLY,
    INPLACE_MODULO,
    STORE_SUBSCR,
    DELETE_SUBSCR,
    BINARY_LSHIFT,
    BINARY_RSHIFT,
    BINARY_AND,
    BINARY_XOR,
    BINARY_OR,
    INPLACE_POWER,
    GET_ITER,
    GET_YIELD_FROM_ITER,
    PRINT_EXPR,
    LOAD_BUILD_CLASS,
    YIELD_FROM,
    GET_AWAITABLE,
    LOAD_ASSERTION_ERROR,
    INPLACE_LSHIFT,
    INPLACE_RSHIFT,
    INPLACE_AND,
    INPLACE_XOR,
    INPLACE_OR,
    LIST_TO_TUPLE,
    RETURN_VALUE,
    IMPORT_STAR,
    SETUP_ANNOTATIONS,
    YIELD_VALUE,
    POP_BLOCK,
    POP_EXCEPT,
    STORE_NAME(String),
    DELETE_NAME(String),
    UNPACK_SEQUENCE(u32),
    FOR_ITER(u32),
    UNPACK_EX {
        before: u8,
        after: u8,
    },
    STORE_ATTR(String),
    DELETE_ATTR(String),
    STORE_GLOBAL(String),
    DELETE_GLOBAL(String),
    LOAD_CONST(String), // XXX: Use repr?
    LOAD_NAME(String),
    BUILD_TUPLE(u32),
    BUILD_LIST(u32),
    BUILD_SET(u32),
    BUILD_MAP(u32),
    LOAD_ATTR(String),
    COMPARE_OP(CmpOp),
    IMPORT_NAME(String),
    IMPORT_FROM(String),
    JUMP_FORWARD(u32),
    JUMP_IF_FALSE_OR_POP(u32),
    JUMP_IF_TRUE_OR_POP(u32),
    JUMP_ABSOLUTE(u32),
    POP_JUMP_IF_FALSE(u32),
    POP_JUMP_IF_TRUE(u32),
    LOAD_GLOBAL(String),
    IS_OP(bool),
    CONTAINS_OP(bool),
    JUMP_IF_NOT_EXC_MATCH(u32),
    SETUP_FINALLY(u32),
    LOAD_FAST(String),
    STORE_FAST(String),
    DELETE_FAST(String),
    RAISE_VARARGS(RaiseForm),
    CALL_FUNCTION(u32),
    MAKE_FUNCTION(MakeFunctionFlags),
    BUILD_SLICE(BuildSliceArgc),
    LOAD_CLOSURE(u32),
    LOAD_DEREF(u32),
    STORE_DEREF(u32),
    DELETE_DEREF(u32),
    CALL_FUNCTION_KW(u32),
    CALL_FUNCTION_EX(CallFunctionExFlags),
    SETUP_WITH(u32),
    LIST_APPEND,
    SET_ADD,
    MAP_ADD,
    LOAD_CLASSDEREF(u32),
    SETUP_ASYNC_WITH,
    FORMAT_VALUE,
    BUILD_CONST_KEY_MAP(u32),
    BUILD_STRING(u32),
    LOAD_METHOD,
    CALL_METHOD,
    LIST_EXTEND(u32),
    SET_UPDATE(u32),
    DICT_MERGE(u32),
    DICT_UPDATE(u32),

    BUILD_TUPLE_UNPACK(u32),
    BUILD_TUPLE_UNPACK_WITH_CALL(u32),
    BUILD_LIST_UNPACK(u32),
    BUILD_SET_UNPACK(u32),
    BUILD_MAP_UNPACK(u32),
    BUILD_MAP_UNPACK_WITH_CALL(u32),
}
