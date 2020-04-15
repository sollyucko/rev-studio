use pyo3::ffi;

pub enum Instruction {
    Nop,
    Shuffle(u8, [u8; 4]), // 255 = nothing
    UnaryOp(fn(ffi::PyObject) -> ffi::PyObject),
    UnaryConsumer(fn(ffi::PyObject) -> ()),
    NonConsumingUnaryOp(fn(ffi::PyObject) -> ffi::PyObject),
    BinaryOp(fn(ffi::PyObject, ffi::PyObject) -> ffi::PyObject),
    BinaryConsumer(fn(ffi::PyObject, ffi::PyObject) -> ()),
    TernaryConsumer(fn(ffi::PyObject, ffi::PyObject, ffi::PyObject) -> ()),
    SeptenaryConsumer(fn(ffi::PyObject, ffi::PyObject, ffi::PyObject, ffi::PyObject, ffi::PyObject, ffi::PyObject, ffi::PyObject) -> ()),
}
