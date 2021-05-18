//! See [RFC as Readme-For-Crate](https://github.com/oooutlk/structx/blob/main/README.md)
//! for more.
//! 
//! # Overview
//! 
//! This project provides simulation of anonymous struct and named arguments in
//! Rust, using proc macros `structx!{}`, `Structx!{}`, `#[named_args]` and
//! `args!{}`.
//! 
//! ## Usage of this crate
//! 
//! Add the following in your Cargo.toml file:
//! 
//! ```toml
//! [dependencies]
//! structx = "0.1"
//! 
//! [package.metadata.inwelling]
//! structx = true
//! ```
//! 
//! Add the following in your .rs files:
//! 
//! ```rust,no_run
//! use structx::*;
//! ```
//! 
//! If you want to use named arguments, add the following:
//! 
//! ```rust,no_run
//! use structx::named_args::*;
//! ```
//! 
//! # Definitions and notations of anonymous structs
//! 
//! Anonymous structs are `struct`s without the needs of providing struct names.
//! However, the field names are mandatory. Anonymous structs are of the same type
//! if and only if they are composed of the same set of field names. The order of
//! fields are irrelevant.
//! 
//! ## Value of anonymous structs
//! 
//! The notation of an anonymous struct's value is `structx!{}`.
//! 
//! ## Examples of anonymous struct's values
//! 
//! ```rust,no_run
//! let foo = structx!{ i: 3, b: true };
//! let bar = structx!{ x, y };
//! ```
//! 
//! ## Type of anonymous structs
//! 
//! The notation of an anonymous struct's type is `Structx!{}`.
//! 
//! ## Examples of anonymous struct's types
//! 
//! ```rust,no_run
//! fn foo( x: i32, y: i32 ) -> Structx!{ x: i32, y: i32 } {
//!     structx!{ x, y: y+1 }
//! }
//! ```
//! ## Traits derived for anonymous structs
//!
//! Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash.
//!
//! ```rust
//! let a = structx!{ width :  800, height: 600 };
//! let b = structx!{ height:  600, width : 800 };
//! let c = structx!{ width : 1024, height: 768 };
//! assert_eq!( a, b );
//! assert_ne!( a, c );
//! ```
//!
//! # Simulation of named arguments
//! 
//! At definition site, add attributes `#[named_args]` to functions.
//! 
//! ```rust,no_run
//! #[named_args]
//! fn set_size( width: u32, height: u32 ) { todo!() }
//! ```
//! 
//! At call site, wrap arguments with `args!{}`.
//! 
//! ```rust,no_run
//! set_size( args!{ width: 1024, height: 768 });
//! ```
//!
//! # License
//! 
//! Under Apache License 2.0 or MIT License, at your will.

pub use structx_derive::{
    Structx,
    structx,
};

/// Simulating named arguments.
/// `#[named_args]` is for functions with named arguments.
/// `arg!{}` is an alias for `structx!{}`.
pub mod named_args {
    pub use structx_derive::{
        named_args,
        structx as args,
    };
}

include!( concat!( env!( "OUT_DIR" ), "/bindings.rs" ));
