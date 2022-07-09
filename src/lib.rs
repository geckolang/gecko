#![deny(rust_2018_idioms)]

pub mod ast;
pub mod cache;
pub mod type_system;
pub mod lexer;
pub mod borrow_check;
pub mod lint;
pub mod lowering;
mod mock;
pub mod name_resolution;
pub mod parser;
pub mod visitor;
