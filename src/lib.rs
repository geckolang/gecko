#![deny(rust_2018_idioms)]

pub mod ast;
pub mod borrow_check;
pub mod cache;
pub mod lexer;
pub mod lint;
pub mod lowering;
mod mock;
pub mod name_resolution;
pub mod parser;
pub mod type_check;
pub mod type_inference;
pub mod type_system;
pub mod visitor;
