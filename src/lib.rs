#![deny(rust_2018_idioms)]

pub mod ast;
pub mod context;
pub mod diagnostic;
pub mod lexer;
pub mod llvm_lowering;
pub mod name_resolution;
pub mod parser;
pub mod token;
pub mod type_check;
