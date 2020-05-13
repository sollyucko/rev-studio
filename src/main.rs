/*use std::io::BufRead;
use eclectic::{List, Map};*/

/*
pub mod archive_format {
    pub mod none;
    pub mod folder;
    pub mod zip;
    pub mod jar;
    pub mod apk;
}

pub mod file_format {
    pub mod elf;
    pub mod exe;
    pub mod java_class;
    pub mod dex;
    pub mod pyc;
}

pub mod bytecode_format {
    pub mod asm {
        pub mod x86_64;
        // TODO: etc.
    }
    pub mod java;
    pub mod dex {
        pub mod dex;
        pub mod odex;
    }
    pub mod python;
    pub mod dotnet {
        pub mod cil;
    }
    pub mod llvm;
    pub mod rust {
        pub mod mir;
        pub mod hir;
    }
}
*/

/*trait FileData : From<BufRead> {
    fn from<R : BufRead>(reader: R) -> Self where Self : Sized;
}

trait ArchiveOf<F : FileData, M: Map<Key=String, Value=F>> : FileData {
    fn get_files(&self) -> M;
}

trait BytecodeInstr {}

trait Bytecode<B : BytecodeInstr>: List<Item=B> {}*/

fn main() {
    println!("Hello, world!");
}
