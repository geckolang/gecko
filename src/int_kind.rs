use crate::node;

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone, std::cmp::PartialOrd)]
pub enum IntSize {
  Size8,
  Size16,
  Size32,
  Size64,
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct IntKind {
  pub size: IntSize,
  pub is_signed: bool,
}

impl<'a> node::Node for IntKind {
  //
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct BoolKind;

impl<'a> node::Node for BoolKind {
  //
}

// TODO: Add more test cases for larger numbers than `0`. Also, is there a need for a panic here? If so, consider using `unreachable!()`. Additionally, should `unreachabe!()` panics even be reported on the documentation?
/// Determine the minimum bit-size in which a number can fit.
///
/// # Examples
///
/// ```
/// use gecko::int_kind::*;
///
/// assert_eq!(minimum_int_size_of(&0), IntSize::Size8);
/// ```
///
/// # Panics
///
/// Panics if the calculated minimum bit-size exceeds 64.
pub fn minimum_int_size_of(number: &u64) -> IntSize {
  let log2_result = f64::log2(*number as f64 + 1_f64);
  let minimum_bit_size = f64::floor(log2_result) as u64;

  if minimum_bit_size <= 8 {
    IntSize::Size8
  } else if minimum_bit_size <= 16 {
    IntSize::Size16
  } else if minimum_bit_size <= 32 {
    IntSize::Size32
  } else if minimum_bit_size <= 64 {
    IntSize::Size64
  } else {
    panic!("expected calculated minimum bit-size to be smaller than 64");
  }
}
