/// The never type represents a type in Rust which has no values therefore it will never appear
/// at runtime.
pub enum Never {}

impl Never {
    /// The `Never` type won’t exist at runtime. Call this function on a `Never` type to make sure
    /// a program that has a `Never` type checks.
    pub fn unreachable(&self) -> ! {
        unreachable!()
    }
}
