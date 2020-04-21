#[macro_export]
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
