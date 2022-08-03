#![deny(rust_2018_idioms)]

pub mod ast;
pub mod lexer;
pub mod lowering;
mod test;
pub mod name_resolution;
pub mod parser;
pub mod semantic_check;
pub mod symbol_table;
pub mod type_inference;
pub mod visitor;
