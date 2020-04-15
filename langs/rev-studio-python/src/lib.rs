// Based on:
//  https://nedbatchelder.com/blog/200804/the_structure_of_pyc_files.html
//  https://github.com/python/cpython/blob/master/Lib/importlib/_bootstrap_external.py
//  https://github.com/python/cpython/blob/master/Lib/dis.py
//  https://github.com/python/cpython/blob/master/Lib/opcode.py
//  https://github.com/python/cpython/blob/master/Python/compile.c
//  https://github.com/python/cpython/blob/master/Python/ceval.c
//  https://docs.python.org/X.X/library/dis.html
// Other tools:
//  https://regexr.com/

#![allow(dead_code)]

use bitfield::bitfield;
use pyo3::{
    ffi,
    marshal::loads,
    AsPyPointer, PyDowncastError, PyErr, Python,
};

use std::io::{self, BufRead, Seek, SeekFrom};
use std::time::{Duration, SystemTime};

pub mod instructions;

macro_rules! union {
    ($vis:vis $name:ident $([$attr:meta])* $($variant:ident($type:ty),)+) => {
        $(#[$attr])*
        $vis enum $name {
            $($variant($type),)+
        }

        $(impl From<$type> for $name {
            fn from(orig: $type) -> $name {
                $name::$variant(orig)
            }
        })+
    }
}

#[derive(Debug)]
pub enum PycParseError {
    MagicNumberShouldEndIn0d0a([u8; 0x2]),
    UnrecognizedPythonVersion([u8; 0x2]),
    UnrecognizedFlag(u32),
}

union! { pub PycError [derive(Debug)]
    ParseError(PycParseError),
    IoError(io::Error),
    PyErr(PyErr),
    PyDowncastError(PyDowncastError),
}

type PycResult<T> = Result<T, PycError>;

bitfield! {
    pub struct Flags(u32);
    impl Debug;
    pub is_hash_based, _: 0;
    pub is_check_source, _: 1;
}

// TODO (eventually): different for different Python versions -> enum?
pub struct PycMetadata {
    pub version: &'static str,
    pub flags: Flags,
    pub mtime: SystemTime,
    pub source_size: u32,
}

pub struct Pyc<'a> {
    metadata: PycMetadata,
    code: MyPyCodeObject<'a>,
}

impl Pyc<'_> {
    fn is_likely_valid<F: BufRead + Seek>(f: &mut F) -> PycResult<()> {
        Pyc::read_metadata(f)?;
        Ok(())
    }

    fn read_metadata<F: BufRead + Seek>(f: &mut F) -> PycResult<PycMetadata> {
        f.seek(SeekFrom::Start(0x0))?;
        let mut buf = [0; 0x4];

        f.read_exact(&mut buf)?;
        let version = match buf {
            [a, b, 0x0d, 0x0a] => {
                Pyc::detect_version_from_magic_number((b as u16) * 0x100u16 + (a as u16))
                    .ok_or(PycParseError::UnrecognizedPythonVersion([a, b]))?
            }
            [_, _, c, d] => Err(PycParseError::MagicNumberShouldEndIn0d0a([c, d]))?,
        };

        f.read_exact(&mut buf)?;
        let flags_u32 = u32::from_le_bytes(buf);
        if flags_u32 & !0b11u32 != 0 {
            Err(PycParseError::UnrecognizedFlag(flags_u32))?;
        };
        let flags = Flags(flags_u32);

        f.read_exact(&mut buf)?;
        let mtime = SystemTime::UNIX_EPOCH + Duration::from_secs(u32::from_le_bytes(buf) as u64);

        f.read_exact(&mut buf)?;
        let source_size = u32::from_le_bytes(buf);

        Ok(PycMetadata {
            version,
            flags,
            mtime,
            source_size,
        })
    }

    fn detect_version_from_magic_number(magic_number: u16) -> Option<&'static str> {
        #[allow(unreachable_patterns)]
        match magic_number {
            20121 => Some("Python 1.5"),
            20121 => Some("Python 1.5.1"),
            20121 => Some("Python 1.5.2"),
            50428 => Some("Python 1.6"),
            50823 => Some("Python 2.0"),
            50823 => Some("Python 2.0.1"),
            60202 => Some("Python 2.1"),
            60202 => Some("Python 2.1.1"),
            60202 => Some("Python 2.1.2"),
            60717 => Some("Python 2.2"),
            62011 => Some("Python 2.3a0"),
            62021 => Some("Python 2.3a0"),
            62011 => Some("Python 2.3a0"),
            62041 => Some("Python 2.4a0"),
            62051 => Some("Python 2.4a3"),
            62061 => Some("Python 2.4b1"),
            62071 => Some("Python 2.5a0"),
            62081 => Some("Python 2.5a0"),
            62091 => Some("Python 2.5a0"),
            62092 => Some("Python 2.5a0"),
            62101 => Some("Python 2.5b3"),
            62111 => Some("Python 2.5b3"),
            62121 => Some("Python 2.5c1"),
            62131 => Some("Python 2.5c2"),
            62151 => Some("Python 2.6a0"),
            62161 => Some("Python 2.6a1"),
            62171 => Some("Python 2.7a0"),
            62181 => Some("Python 2.7a0"),
            62191 => Some("Python 2.7a0"),
            62201 => Some("Python 2.7a0"),
            62211 => Some("Python 2.7a0"),
            3000 => Some("Python 3000"),
            3010 => Some("Python 3000"),
            3020 => Some("Python 3000"),
            3030 => Some("Python 3000"),
            3040 => Some("Python 3000"),
            3050 => Some("Python 3000"),
            3060 => Some("Python 3000"),
            3061 => Some("Python 3000"),
            3071 => Some("Python 3000"),
            3081 => Some("Python 3000"),
            3091 => Some("Python 3000"),
            3101 => Some("Python 3000"),
            3103 => Some("Python 3000"),
            3111 => Some("Python 3.0a4"),
            3131 => Some("Python 3.0b1"),
            3141 => Some("Python 3.1a1"),
            3151 => Some("Python 3.1a1"),
            3160 => Some("Python 3.2a1"),
            3170 => Some("Python 3.2a2"),
            3180 => Some("Python 3.2a3"),
            3190 => Some("Python 3.3a1"),
            3200 => Some("Python 3.3a1"),
            3210 => Some("Python 3.3a1"),
            3220 => Some("Python 3.3a2"),
            3230 => Some("Python 3.3a4"),
            3250 => Some("Python 3.4a1"),
            3260 => Some("Python 3.4a1"),
            3270 => Some("Python 3.4a1"),
            3280 => Some("Python 3.4a1"),
            3290 => Some("Python 3.4a4"),
            3300 => Some("Python 3.4a4"),
            3320 => Some("Python 3.5a1"),
            3330 => Some("Python 3.5b1"),
            3340 => Some("Python 3.5b2"),
            3350 => Some("Python 3.5b3"),
            3351 => Some("Python 3.5.2"),
            3360 => Some("Python 3.6a0"),
            3361 => Some("Python 3.6a1"),
            3370 => Some("Python 3.6a2"),
            3371 => Some("Python 3.6a2"),
            3372 => Some("Python 3.6a2"),
            3373 => Some("Python 3.6b1"),
            3375 => Some("Python 3.6b1"),
            3376 => Some("Python 3.6b1"),
            3377 => Some("Python 3.6b1"),
            3378 => Some("Python 3.6b2"),
            3390 => Some("Python 3.7a1"),
            3391 => Some("Python 3.7a2"),
            3392 => Some("Python 3.7a4"),
            3393 => Some("Python 3.7b1"),
            3394 => Some("Python 3.7b5"),
            3400 => Some("Python 3.8a1"),
            3401 => Some("Python 3.8a1"),
            3410 => Some("Python 3.8a1"),
            3411 => Some("Python 3.8b2"),
            3412 => Some("Python 3.8b2"),
            3413 => Some("Python 3.8b4"),
            3420 => Some("Python 3.9a0"),
            3421 => Some("Python 3.9a0"),
            3422 => Some("Python 3.9a0"),
            3423 => Some("Python 3.9a2"),
            3424 => Some("Python 3.9a2"),
            3425 => Some("Python 3.9a2"),
            _ => None,
        }
    }

    fn try_parse<F: BufRead + Seek>(f: &mut F) -> PycResult<Pyc> {
        let metadata = Pyc::read_metadata(f)?;
        let mut code_buffer = Vec::new();
        f.read_to_end(&mut code_buffer)?;
        let gil_guard = Python::acquire_gil();
        let py = gil_guard.python();

        let code_ptr = loads(py, &code_buffer)?.as_ptr() as *mut ffi::PyCodeObject;
        // This should be valid, since ffi::PyCodeObject is Copy, as long as the refcount is positive
        // (effectively until the end of this scope)
        let raw_code = unsafe { *code_ptr };
        let code = panic!(); // TODO: convert raw_code to code
        Ok(Pyc { metadata, code })
    }
}

// TODO: add past instructions
#[allow(non_camel_case_types)]
enum PyOpCode {
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

enum CmpOp {
    Lt, // <
    Le, // <=
    Eq, // ==
    Ne, // !=
    Gt, // >
    Ge, // >=
}

enum RaiseForm {
    ReRaise,
    Raise,
    RaiseFrom,
}

bitfield! {
    pub struct MakeFunctionFlags(u8);
    impl Debug;
    pub has_positional_defaults, _: 0;
    pub has_kwonly_defaults, _: 1;
    pub has_annotations, _: 2;
    pub has_freevars, _: 3;
}

bitfield! {
    pub struct CallFunctionExFlags(u8);
    impl Debug;
    pub has_kwargs, _: 0;
}


/// The order is arbitrary and may change at any time
#[allow(non_camel_case_types)]
enum PyInstruction<'a> {
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
	STORE_NAME(&'a str),
	DELETE_NAME(&'a str),
	UNPACK_SEQUENCE(u32),
	FOR_ITER(u32), /// delta
	UNPACK_EX(u8, u8), /// counts
	STORE_ATTR(&'a str),
	DELETE_ATTR(&'a str),
	STORE_GLOBAL(&'a str),
	DELETE_GLOBAL(&'a str),
	LOAD_CONST(&'a str), // XXX: Use repr?
	LOAD_NAME(&'a str),
	BUILD_TUPLE(u32), /// count
	BUILD_LIST(u32), /// count
	BUILD_SET(u32), /// count
	BUILD_MAP(u32), /// count
	LOAD_ATTR(&'a str),
	COMPARE_OP(CmpOp),
	IMPORT_NAME(&'a str),
	IMPORT_FROM(&'a str),
	JUMP_FORWARD(u32), /// delta
	JUMP_IF_FALSE_OR_POP(u32), /// target
	JUMP_IF_TRUE_OR_POP(u32), /// target
	JUMP_ABSOLUTE(u32), /// target
	POP_JUMP_IF_FALSE(u32), /// target
	POP_JUMP_IF_TRUE(u32), /// target
	LOAD_GLOBAL(&'a str),
	IS_OP(bool),
	CONTAINS_OP(bool),
	JUMP_IF_NOT_EXC_MATCH(u32), /// target
	SETUP_FINALLY(u32), /// delta
	LOAD_FAST(&'a str),
	STORE_FAST(&'a str),
	DELETE_FAST(&'a str),
	RAISE_VARARGS(RaiseForm),
	CALL_FUNCTION(u32),
	MAKE_FUNCTION(MakeFunctionFlags),
	BUILD_SLICE(u8),
	LOAD_CLOSURE(u32),
	LOAD_DEREF(u32), /// slot (cell & free)
	STORE_DEREF(u32), /// slot (cell & free)
	DELETE_DEREF(u32), /// slot (cell & free)
	CALL_FUNCTION_KW(u32), /// argc
	CALL_FUNCTION_EX(CallFunctionExFlags),
	SETUP_WITH(u32), /// delta
	LIST_APPEND,
	SET_ADD,
	MAP_ADD,
	LOAD_CLASSDEREF(u32), /// slot (cell & free), check locals first
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

struct MyPyCodeObject<'a> {
    argcount: i32,
    posonlyargcount: i32,
    kwonlyargcount: i32,
    nlocals: i32,
    stacksize: i32,
    flags: i32,
    firstlineno: i32,
    code: &'a [u8],
    consts: &'a [ffi::PyObject],
    names: &'a [&'a str],
    varnames: &'a [&'a str],
    freevars: &'a [&'a str],
    cellvars: &'a [&'a str],
    filename: &'a str,
    name: &'a str,
    lnotab: &'a [u8],
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parsing_pyc() -> PycResult<()> {
        // xxd -i
        let mut test_file = Cursor::<&[u8]>::new(&[
            0x42, 0x0d, 0x0d, 0x0a, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x1a, 0x78, 0x5e, 0x00, 0x00,
            0x00, 0x00, 0xe3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x01, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x73, 0x04, 0x00, 0x00, 0x00,
            0x64, 0x00, 0x53, 0x00, 0x29, 0x01, 0x4e, 0xa9, 0x00, 0x72, 0x01, 0x00, 0x00, 0x00,
            0x72, 0x01, 0x00, 0x00, 0x00, 0x72, 0x01, 0x00, 0x00, 0x00, 0xfa, 0x53, 0x2f, 0x68,
            0x6f, 0x6d, 0x65, 0x2f, 0x73, 0x6f, 0x6c, 0x6c, 0x79, 0x2f, 0x44, 0x6f, 0x63, 0x75,
            0x6d, 0x65, 0x6e, 0x74, 0x73, 0x2f, 0x43, 0x6f, 0x64, 0x65, 0x2f, 0x52, 0x75, 0x73,
            0x74, 0x2f, 0x72, 0x65, 0x76, 0x2d, 0x73, 0x74, 0x75, 0x64, 0x69, 0x6f, 0x2f, 0x6c,
            0x61, 0x6e, 0x67, 0x73, 0x2f, 0x72, 0x65, 0x76, 0x2d, 0x73, 0x74, 0x75, 0x64, 0x69,
            0x6f, 0x2d, 0x70, 0x79, 0x74, 0x68, 0x6f, 0x6e, 0x2f, 0x74, 0x65, 0x73, 0x74, 0x5f,
            0x72, 0x65, 0x73, 0x2f, 0x74, 0x65, 0x73, 0x74, 0x2e, 0x70, 0x79, 0xda, 0x08, 0x3c,
            0x6d, 0x6f, 0x64, 0x75, 0x6c, 0x65, 0x3e, 0x01, 0x00, 0x00, 0x00, 0xf3, 0x00, 0x00,
            0x00, 0x00,
        ]);
        Pyc::try_parse(&mut test_file)?;
        Ok(())
    }
}
