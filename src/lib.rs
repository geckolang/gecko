#![deny(rust_2018_idioms)]

pub mod ast;
pub mod cache;
pub mod diagnostic;
pub mod lexer;
pub mod lint;
pub mod llvm_lowering;
pub mod name_resolution;
pub mod parser;
pub mod semantic_check;
