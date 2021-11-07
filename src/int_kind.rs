use crate::node;
use crate::pass;

// TODO: Write documentation.
pub fn calculate_int_size_of(number: &u64) -> IntSize {
  let log2_result = f64::log2(*number as f64 + 1_f64);
  let minimum_bit_size = f64::floor(log2_result) as u64;

  if minimum_bit_size <= 8 {
    IntSize::Bit8
  } else if minimum_bit_size <= 16 {
    IntSize::Bit16
  } else if minimum_bit_size <= 32 {
    IntSize::Bit32
  } else if minimum_bit_size <= 64 {
    IntSize::Bit64
  } else {
    panic!("unexpected minimum bit size to be larger than 64");
  }
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone, std::cmp::PartialOrd)]
pub enum IntSize {
  Bit8,
  Bit16,
  Bit32,
  Bit64,
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct IntKind {
  pub size: IntSize,
  pub is_signed: bool,
}

impl node::Node for IntKind {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_int_kind(self)?;

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct BoolKind;

impl node::Node for BoolKind {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_bool_kind(self)?;

    Ok(())
  }
}
