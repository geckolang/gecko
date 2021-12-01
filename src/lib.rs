#![deny(rust_2018_idioms)]

pub mod diagnostic;
pub mod entry_point_check_pass;
pub mod int_kind;
pub mod lexer;
pub mod llvm_lowering_pass;
pub mod name_resolution;
pub mod node;
pub mod parser;
pub mod pass;
pub mod pass_manager;
pub mod token;
pub mod type_check_pass;
