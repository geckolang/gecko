#![deny(rust_2018_idioms)]

pub mod diagnostic;
pub mod int_kind;
pub mod lexer;
pub mod llvm_lowering;
pub mod name_resolution;
pub mod node;
pub mod parser;
pub mod token;
pub mod type_check;
