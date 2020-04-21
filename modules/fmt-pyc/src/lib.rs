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

use bitflags::bitflags;
use pyo3::{ffi, types, PyDowncastError, PyErr, PyObject, PyResult, Python};

use std::io::{self, BufRead, Seek, SeekFrom};
use std::mem::transmute;
use std::os::raw::c_char;
use std::rc::Rc;
use std::slice;
use std::time::{Duration, SystemTime};

use rev_studio_utils::union;

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

bitflags! {
    pub struct Flags: u32 {
        const HASH_BASED   = 0b0000_0001;
        const CHECK_SOURCE = 0b0000_0010;
    }
}

// TODO (eventually): different for different Python versions -> enum?
#[derive(Debug)]
pub struct PycMetadata {
    pub version: &'static str,
    pub flags: Flags,
    pub mtime: SystemTime,
    pub source_size: u32,
}

#[derive(Debug)]
pub struct Pyc {
    pub metadata: PycMetadata,
    pub code: CodeObject,
}

impl Pyc {
    fn is_likely_valid<F: BufRead + Seek>(f: &mut F) -> PycResult<()> {
        Self::read_metadata(f)?;
        Ok(())
    }

    fn read_metadata<F: BufRead + Seek>(f: &mut F) -> PycResult<PycMetadata> {
        f.seek(SeekFrom::Start(0x0))?;
        let mut buf = [0; 0x4];

        f.read_exact(&mut buf)?;
        let version = match buf {
            [a, b, 0x0d, 0x0a] => {
                Self::detect_version_from_magic_number(u16::from(b) * 0x100_u16 + u16::from(a))
                    .ok_or(PycParseError::UnrecognizedPythonVersion([a, b]))?
            }
            [_, _, c, d] => return Err(PycParseError::MagicNumberShouldEndIn0d0a([c, d]).into()),
        };

        f.read_exact(&mut buf)?;
        let flags_u32 = u32::from_le_bytes(buf);
        let flags = Flags::from_bits(flags_u32).ok_or_else(|| PycParseError::UnrecognizedFlag(flags_u32))?;

        f.read_exact(&mut buf)?;
        let mtime =
            SystemTime::UNIX_EPOCH + Duration::from_secs(u64::from(u32::from_le_bytes(buf)));

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
        #[allow(clippy::match_same_arms)]
        #[allow(clippy::match_overlapping_arm)]
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

    fn try_parse<F: BufRead + Seek>(f: &mut F) -> PycResult<Self> {
        let metadata = Self::read_metadata(f)?;
        let mut code_buffer = Vec::new();
        f.read_to_end(&mut code_buffer)?;
        let _gil_guard = Python::acquire_gil();

        // PyMarshal_ReadObjectFromString does not store the string pointer given to it after it
        // returns; it uses memcpy as needed.
        #[allow(clippy::cast_possible_wrap)]
        let raw_code_ptr = unsafe {
            ffi::PyMarshal_ReadObjectFromString(
                code_buffer.as_ptr() as *const c_char,
                code_buffer.len() as isize,
            )
        } as *mut ffi::PyCodeObject;

        let code = unsafe { code_object_from_ffi_py_code_object_ptr(raw_code_ptr) }?;
        Ok(Self { metadata, code })
    }
}

bitflags! {
    pub struct CodeFlags: u32 {
		const OPTIMIZED                   = 0x1;
		const NEWLOCALS                   = 0x2;
		const VARARGS                     = 0x4;
		const VARKEYWORDS                 = 0x8;
		const NESTED                     = 0x10;
		const GENERATOR                  = 0x20;
		const NOFREE                     = 0x40;
		const COROUTINE                  = 0x80;
		const ITERABLE_COROUTINE        = 0x100;
		const ASYNC_GENERATOR           = 0x200;
        // TODO: old versions
        const GENERATOR_ALLOWED        = 0x1000;
        const FUTURE_DIVISION          = 0x2000;
        const FUTURE_ABSOLUTE_IMPORT   = 0x4000;
        const FUTURE_WITH_STATEMENT    = 0x8000;
        const FUTURE_PRINT_FUNCTION   = 0x10000;
        const FUTURE_UNICODE_LITERALS = 0x20000;
        const FUTURE_BARRY_AS_BDFL    = 0x40000;
        const FUTURE_GENERATOR_STOP   = 0x80000;
        #[allow(clippy::unreadable_literal)]
        const FUTURE_ANNOTATIONS     = 0x100000;
    }
}

#[derive(Debug)]
pub struct CodeObject {
    pub argcount: u32,
    //posonlyargcount: i32, // too new
    pub kwonlyargcount: u32,
    pub nlocals: u32,
    pub stacksize: u32,
    pub flags: CodeFlags,
    pub firstlineno: u32,
    pub code: Vec<u8>,
    pub consts: Vec<PyObject>,
    pub names: Vec<String>,
    pub varnames: Vec<String>,
    pub freevars: Vec<String>,
    pub cellvars: Vec<String>,
    //cell2arg: Vec<u8>, // not marshalled
    pub filename: String,
    pub name: String,
    pub lnotab: Vec<u8>,
}

#[allow(clippy::cast_sign_loss)]
unsafe fn code_object_from_ffi_py_code_object_ptr(
    raw_code_ptr: *mut ffi::PyCodeObject,
) -> PyResult<CodeObject> {
    Ok(CodeObject {
        argcount: (*raw_code_ptr).co_argcount as u32,
        //posonlyargcount: (*raw_code_ptr).co_posonlyargcount,
        kwonlyargcount: (*raw_code_ptr).co_kwonlyargcount as u32,
        nlocals: (*raw_code_ptr).co_nlocals as u32,
        stacksize: (*raw_code_ptr).co_stacksize as u32,
        flags: CodeFlags::from_bits_truncate((*raw_code_ptr).co_flags as u32),
        firstlineno: (*raw_code_ptr).co_firstlineno as u32,
        code: vec_u8_from_ffi_pybytes_ptr((*raw_code_ptr).co_code),
        consts: vec_pyobject_from_ffi_pytuple_ptr((*raw_code_ptr).co_consts),
        names: vec_string_from_ffi_pytuple_pystring((*raw_code_ptr).co_names)?,
        varnames: vec_string_from_ffi_pytuple_pystring((*raw_code_ptr).co_varnames)?,
        freevars: vec_string_from_ffi_pytuple_pystring((*raw_code_ptr).co_freevars)?,
        cellvars: vec_string_from_ffi_pytuple_pystring((*raw_code_ptr).co_cellvars)?,
        filename: string_from_ffi_string((*raw_code_ptr).co_filename)?,
        name: string_from_ffi_string((*raw_code_ptr).co_name)?,
        lnotab: vec_u8_from_ffi_pybytes_ptr((*raw_code_ptr).co_lnotab),
    })
}

#[allow(clippy::cast_sign_loss)]
unsafe fn vec_u8_from_ffi_pybytes_ptr(obj: *mut ffi::PyObject) -> Vec<u8> {
    let data = ffi::PyBytes_AsString(obj) as *mut u8;
    let len = ffi::PyBytes_Size(obj) as usize;
    let slice_ = slice::from_raw_parts_mut(data, len);
    slice_.to_vec()
}

#[allow(clippy::cast_sign_loss)]
unsafe fn slice_ffi_pyobject_ptr_from_ffi_pytuple_ptr<'a>(
    obj: *mut ffi::PyObject,
) -> &'a [*mut ffi::PyObject] {
    let data = &mut ((*(obj as *mut ffi::PyTupleObject)).ob_item) as *mut [*mut ffi::PyObject; 1]
        as *mut *mut ffi::PyObject;
    let len = ffi::PyTuple_Size(obj) as usize;
    slice::from_raw_parts_mut(data, len)
}

unsafe fn vec_ffi_pyobject_ptr_from_ffi_pytuple_ptr(
    obj: *mut ffi::PyObject,
) -> Vec<*mut ffi::PyObject> {
    slice_ffi_pyobject_ptr_from_ffi_pytuple_ptr(obj).to_vec()
}

unsafe fn vec_pyobject_from_ffi_pytuple_ptr(obj: *mut ffi::PyObject) -> Vec<PyObject> {
    slice_ffi_pyobject_ptr_from_ffi_pytuple_ptr(obj)
        .iter()
        .map(|o: &*mut ffi::PyObject| {
            ffi::Py_INCREF(*o); // pyo3::PyObject is an owned reference that decrefs after drop
            transmute::<*mut ffi::PyObject, PyObject>(*o) // PyObject isn't Copy, so we can't pointer-cast
        })
        .collect::<Vec<_>>()
}

unsafe fn string_from_ffi_string(obj: *mut ffi::PyObject) -> PyResult<String> {
    Ok(
        (&*(&obj as *const *mut ffi::PyObject as *const types::PyString))
            .to_string()?
            .into_owned(),
    )
}

unsafe fn vec_string_from_ffi_pytuple_pystring(obj: *mut ffi::PyObject) -> PyResult<Vec<String>> {
    slice_ffi_pyobject_ptr_from_ffi_pytuple_ptr(obj)
        .iter()
        .map(|o: &*mut ffi::PyObject| string_from_ffi_string(*o))
        .collect::<Result<Vec<_>, _>>()
}

impl CodeObject {
    pub fn iter_code_rc(self: Rc<Self>) -> impl Iterator<Item = u8> + Clone {
        IterCodeRc { code_obj: self, i: 0 }
    }
}

#[derive(Clone, Debug)]
struct IterCodeRc {
    code_obj: Rc<CodeObject>,
    i: usize,
}

impl Iterator for IterCodeRc {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        if self.i >= self.code_obj.code.len() {
            None
        } else {
            let result = self.code_obj.code[self.i];
            self.i += 1;
            Some(result)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

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
        println!("{:?}", Pyc::try_parse(&mut test_file)?);
        Ok(())
    }
}
