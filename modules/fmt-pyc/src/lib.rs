#![allow(clippy::wildcard_imports)]
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

use std::io::Read;
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use py_marshal::{
    self,
    read::{marshal_load_ex, MarshalLoadExOptions},
    Code,
};

pub use self::errors::*;

/// Contains [`Error`], [`ErrorKind`], [`Result`], and [`ResultExt`].
pub mod errors {
    use error_chain::error_chain;

    error_chain! {
        types {
            Error, ErrorKind, ResultExt, Result;
        }

        links {
            MarshalRead(::py_marshal::read::errors::Error, ::py_marshal::read::errors::ErrorKind);
        }

        foreign_links {
            Io(::std::io::Error);
        }

        errors {
            MagicNumberShouldEndIn0d0a(end_of_magic: [u8; 0x2])
            UnrecognizedPythonVersion(version_id: u16)
            UnrecognizedFlag(flag: u32)
            Extract(obj: ::py_marshal::Obj)
        }

        skip_msg_variant
    }
}

bitflags! {
    /// .pyc flags
    pub struct Flags: u32 {
        const HASH_BASED   = 0b0000_0001;
        const CHECK_SOURCE = 0b0000_0010;
    }
}

// TODO (eventually): different for different Python versions -> enum?
/// .pyc metadata
#[derive(Debug)]
pub struct PycMetadata {
    pub version_id: u16,
    pub flags: Flags,
    pub mtime: SystemTime,
    pub source_size: u32,
}

/// A parsed .pyc file
#[derive(Debug)]
pub struct Pyc {
    pub metadata: PycMetadata,
    pub code: Arc<Code>,
}

impl Pyc {
    /// Checks if the given file seems to be a .pyc file.
    /// # Errors
    /// Returns:
    /// - `Ok(())` if the file seems to be a .pyc file.
    /// - `Err(...)` if the file is not recognized as a valid .pyc file.
    pub fn is_likely_valid<F: Read>(f: &mut F) -> Result<()> {
        let _: PycMetadata = Self::read_metadata(f)?;
        Ok(())
    }

    fn read_metadata<F: Read>(f: &mut F) -> Result<PycMetadata> {
        let mut buf = [0; 0x4];

        f.read_exact(&mut buf)?;
        let version_id = match buf {
            [a, b, 0x0d, 0x0a] => {
                let x = u16::from_le_bytes([a, b]);
                let _: &'static str =
                    Self::version_id_to_string(x).ok_or(ErrorKind::UnrecognizedPythonVersion(x))?;
                x
            }
            [_, _, c, d] => return Err(ErrorKind::MagicNumberShouldEndIn0d0a([c, d]).into()),
        };

        f.read_exact(&mut buf)?;
        let flags_u32 = u32::from_le_bytes(buf);
        let flags =
            Flags::from_bits(flags_u32).ok_or_else(|| ErrorKind::UnrecognizedFlag(flags_u32))?;

        f.read_exact(&mut buf)?;
        let mtime =
            SystemTime::UNIX_EPOCH + Duration::from_secs(u64::from(u32::from_le_bytes(buf)));

        f.read_exact(&mut buf)?;
        let source_size = u32::from_le_bytes(buf);

        Ok(PycMetadata {
            version_id,
            flags,
            mtime,
            source_size,
        })
    }

    /// Converts the given .pyc version id into a version string.
    /// # Errors
    /// Returns:
    /// - `Some(...)` if the version id corresponds to an known Python version.
    /// - `None` if the version id is unrecognized.
    #[must_use]
    pub fn version_id_to_string(version_id: u16) -> Option<&'static str> {
        #[allow(unreachable_patterns)]
        #[allow(clippy::match_same_arms)]
        #[allow(clippy::match_overlapping_arm)]
        match version_id {
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

    /// Parses the given .pyc file.
    /// # Errors
    /// Returns:
    /// - `Ok(...)` if the .pyc file was successfully parsed into metadata and a `code` object.
    /// - `Err(...)` if an error occured while parsing the .pyc file.
    pub fn try_parse<F: Read>(f: &mut F) -> Result<Self> {
        let metadata = Self::read_metadata(f)?;
        let code = marshal_load_ex(
            f,
            MarshalLoadExOptions {
                has_posonlyargcount: metadata.version_id < 20121 && metadata.version_id >= 3411,
            },
        )?
        .extract_code()
        .map_err(ErrorKind::Extract)?;
        Ok(Self { metadata, code })
    }
}

bitflags! {
    /// `code` object flags.
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

#[cfg(test)]
mod test {
    use super::{Pyc, Result};

    #[test]
    fn test_parsing_pyc_37() -> Result<()> {
        // xxd -i
        let mut test_file: &[u8] = &[
            0x42, 0x0d, 0x0d, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x89, 0x06, 0xbc, 0x5e, 0x01, 0x00,
            0x00, 0x00, 0xe3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x01, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x73, 0x04, 0x00, 0x00, 0x00,
            0x64, 0x00, 0x53, 0x00, 0x29, 0x01, 0x4e, 0xa9, 0x00, 0x72, 0x01, 0x00, 0x00, 0x00,
            0x72, 0x01, 0x00, 0x00, 0x00, 0x72, 0x01, 0x00, 0x00, 0x00, 0xfa, 0x49, 0x2f, 0x68,
            0x6f, 0x6d, 0x65, 0x2f, 0x73, 0x6f, 0x6c, 0x6f, 0x6d, 0x6f, 0x6e, 0x75, 0x2f, 0x44,
            0x6f, 0x63, 0x75, 0x6d, 0x65, 0x6e, 0x74, 0x73, 0x2f, 0x43, 0x6f, 0x64, 0x65, 0x2f,
            0x52, 0x75, 0x73, 0x74, 0x2f, 0x72, 0x65, 0x76, 0x2d, 0x73, 0x74, 0x75, 0x64, 0x69,
            0x6f, 0x2f, 0x74, 0x65, 0x73, 0x74, 0x5f, 0x64, 0x61, 0x74, 0x61, 0x2f, 0x70, 0x79,
            0x74, 0x68, 0x6f, 0x6e, 0x5f, 0x6d, 0x69, 0x6e, 0x69, 0x6d, 0x61, 0x6c, 0x2e, 0x70,
            0x79, 0xda, 0x08, 0x3c, 0x6d, 0x6f, 0x64, 0x75, 0x6c, 0x65, 0x3e, 0x01, 0x00, 0x00,
            0x00, 0xf3, 0x00, 0x00, 0x00, 0x00
        ];
        println!("{:?}", Pyc::try_parse(&mut test_file)?);
        Ok(())
    }

    #[test]
    fn test_more_parsing_pyc_37()-> Result<()> {
        let mut test_file: &[u8] = &[
            0x42, 0x0d, 0x0d, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x3b, 0x53, 0xbc, 0x5e, 0x10, 0x00,
            0x00, 0x00, 0xe3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x02, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x73, 0x0c, 0x00, 0x00, 0x00,
            0x65, 0x00, 0x64, 0x00, 0x83, 0x01, 0x01, 0x00, 0x64, 0x01, 0x53, 0x00, 0x29, 0x02,
            0x69, 0x56, 0x34, 0x12, 0x00, 0x4e, 0x29, 0x01, 0xda, 0x05, 0x70, 0x72, 0x69, 0x6e,
            0x74, 0xa9, 0x00, 0x72, 0x02, 0x00, 0x00, 0x00, 0x72, 0x02, 0x00, 0x00, 0x00, 0xfa,
            0x4e, 0x2f, 0x68, 0x6f, 0x6d, 0x65, 0x2f, 0x73, 0x6f, 0x6c, 0x6f, 0x6d, 0x6f, 0x6e,
            0x75, 0x2f, 0x44, 0x6f, 0x63, 0x75, 0x6d, 0x65, 0x6e, 0x74, 0x73, 0x2f, 0x43, 0x6f,
            0x64, 0x65, 0x2f, 0x52, 0x75, 0x73, 0x74, 0x2f, 0x72, 0x65, 0x76, 0x2d, 0x73, 0x74,
            0x75, 0x64, 0x69, 0x6f, 0x2f, 0x74, 0x65, 0x73, 0x74, 0x5f, 0x64, 0x61, 0x74, 0x61,
            0x2f, 0x70, 0x79, 0x74, 0x68, 0x6f, 0x6e, 0x5f, 0x65, 0x78, 0x74, 0x65, 0x6e, 0x64,
            0x65, 0x64, 0x5f, 0x61, 0x72, 0x67, 0x2e, 0x70, 0x79, 0xda, 0x08, 0x3c, 0x6d, 0x6f,
            0x64, 0x75, 0x6c, 0x65, 0x3e, 0x01, 0x00, 0x00, 0x00, 0xf3, 0x00, 0x00, 0x00, 0x00
        ];
        println!("{:?}", Pyc::try_parse(&mut test_file)?);
        Ok(())
    }
}
