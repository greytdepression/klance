//--------------------------------------------------
// Macros
//--------------------------------------------------

#[macro_export]
macro_rules! index_struct {
    ($i:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct $i(usize);
    };
}