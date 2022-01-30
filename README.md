<p align="center">
  <img alt="Logo" width="200" src="https://i.ibb.co/fFtvn08/Gecko-Logo-Logo-Only-01.png" />
  <br/>
  <br/>
  <i>Gecko is a general-purpose programming language built for the LLVM platform.</i>
  <br/>
  <strong align="center">Gecko</strong>
  <br/>
  <br/>
  <img alt="GitHub branch checks state" src="https://img.shields.io/github/checks-status/ionlang/grip/master?style=for-the-badge" />
  <img alt="Discord" src="https://img.shields.io/discord/572951207862206474?label=Discord&style=for-the-badge" />
</p>
<br/>
<hr/>

#### Technology & principles

Gecko is a general-purpose, strongly-typed programming language, with a focus on a powerful type system, memory safety, and simplicity. Inspired by Rust, Java, C#, JavaScript, Scala, and others. Built using Rust. It uses [🔗LLVM](https://llvm.org/) as its backend.

[🔗Join our Discord server](https://discord.gg/H3eMUXp)

Overview:

- [Syntax example](#syntax-example)
- [Project roadmap](#project-roadmap)
- [Directory structure](#directory-structure)
- [Language specification](#language-specification)
- [Building](#building)

#### Syntax example

```rust
extern fn puts(msg: str): i32;

fn main(): i32 {
  unsafe {
    puts("hello world");
  }

  return 0;
}
```

### Project roadmap

_🔨 &mdash; Work in progress._ _✔️ &mdash; Completed._
| Feature | Note(s) | Status |
|--------------------|------------------------------------------------------------|---------|
| Functions | - | ✔️ |
| Externs | - | ✔️ |
| Function calls | - | ✔️ |
| Structs | Definition, declaration & accessing of structs. | 🔨 |
| Global variables | - | 🔨 |
| Modules | - | 🔨 |
| Generics | - | 🔨 |
| `if` statement | Includes the `else` statement as well. | 🔨 |
| `return` statement | - | ✔️ |
| Variables | Declaration, assignment, and reference of variables. | ✔️ |
| Casting | - | 🔨 |
| Binary expressions | - | 🔨 |
| Literals | Includes string, integer, character, and boolean literals. | ✔️ |
| Types | Intrinsic types such as `bool`, `i32`, `str`, etc. | ✔️ |
| Arrays | - | 🔨 |

### Directory structure

| Path         | Description                                                                                    |
| ------------ | ---------------------------------------------------------------------------------------------- |
| `.github/`   | Contains GitHub configuration files. For example, the GitHub actions workflow file `rust.yml`. |
| `src/`       | The root directory for the project's source files.                                             |
| `tests/`     | Root directory for integration tests.                                                          |
| `.gitignore` | Configuration file to specify paths ignored by Git.                                            |
| `Cargo.lock` | Cargo configuration file.                                                                      |
| `Cargo.toml` | Cargo configuration file.                                                                      |
| `LICENSE`    | The license file.                                                                              |
| `README.md`  | Information about the project.                                                                 |

### Language specification

#### &mdash; Identifiers &amp; naming

Naming is straight forward. Whitespace and most special characters are disallowed in names (with the exception of `_`). Identifiers must not start with a number. They must also not be reserved keywords or types.

Here is the exact regular expression rule for identifiers:

```r
^([_a-zA-Z]+[\w]*)
```

[_🔗test this regular expression_](https://regex101.com/r/KDIWdL/1)

#### &mdash; Comments

Only single-line comments are available for simplicity. All comments start with the `#` character, and anything after that is considered part of the comment and is ignored by the compiler.

```py
# This is a comment.
```

It should be noted that string literals take precedence over comments (as one would logically expected):

```
"# This is a string literal."
```

Please note that some code examples might include comments using `//`, this is illegal syntax and it is only used for syntax highlighting on this document.

#### &mdash; Types

Several intrinsic types are defined by the compiler. It is intended for the intrinsic types to be bare-bones, and to have the standard library expand upon them, this allows for easier refactoring of type-specific functions, without having to modify the compiler's source code.

| Definition | Description                                                                                                         |
| ---------- | ------------------------------------------------------------------------------------------------------------------- |
| `bool`     | Boolean type. Its value can either be `true` or `false`.                                                            |
| `str`      | String type. Equivalent to `i8*` or `int*` in other languages.                                                      |
| `i8`       | Integer type with bit-size 8. Can be used to define characters, as well as strings as a pointer.                    |
| `i16`      | Integer type with bit-size 16. Equivalent to a `short int` on other languages.                                      |
| `i32`      | Integer type with bit-size 32. Equivalent to an `int` on other languages. Usually the most common number type used. |
| `i64`      | Integer type with bit-size 64. Equivalent to a `long int` on other languages. Useful for larger numbers.            |

#### &mdash; Modules

Modules provide a simple way of organizing code within a project. They also have the advantage of preventing global naming collisions (ex. when importing a library). Modules are based off the file name, and are to be defined in the `src/` directory. They are not declared in code for simplicity.

Accessing a module is trivial:

```rust
foo.bar.entity
```

#### &mdash; Functions

Function definitions & calls follow conventional norms. They are easy to define and use. The language grammar was designed in a way to have only one way to achieve things, with the idea that limited options remove the problems of different programmers using different methods of accomplishing the same thing (ex. different function declaration syntaxes). This way, whenever you encounter code you know what to expect right away.

Omitting the return type will imply that such function does not return a value (the equivalent to other languages' `void` type). Functions without return types must not return a value, nor are they required to have a `return` statement.

```rust
fn main(argc: i32, argv: i32[]): i32 {
  return 0;
}

fn do_nothing() { }
```

#### &mdash; Variables

Variable declaration, assignment and reference follow straight-forward rules and adhere to common conventions. This makes creating, and using variables easy and most programmers will be familiar with this style. Variable names adhere to the `identifier` rule.

```rust
let product: i32 = 3 * 4;
```

For convenience, variables can also be declared without specifying their types by using the `let` keyword for type inference. When inferring type from a literal integer, the preferred type inferred by the compiler will be `i32`, unless the integer cannot fit into `i32`'s bit-size, in which case it will be either `i64` or `i128` depending on the value's required bit-width. For example, a value larger than `2147483647` will be inferred as `i64` because it cannot fit into `i32`.

Variable declarations are _immutable_ by default, unless the `mut` keyword is used.

```rust
let five: i32 = 5; // type is explicitly given
let inferred_three = 3; // inferred type is `i32`
let big_number_i64 = 2147483647 + 1; // type is inferred to be `i64`
let mut counter = 0; // can be mutated/re-assigned
```

#### &mdash; Loops

There is a single loop construct that can be used to emulate while, for, and infinite loops. The loop construct is a _pre-test loop_, meaning its condition is always evaluated before its body is executed.

You can use the `continue` and `break` keywords as statements inside the loop body to control the loop.

```rs
loop { } // no condition specified: infinite loop (the condition is assumed to be `true`)
loop condition { } // pre-test loop with a condition, emulates a while loop
```

Here's a code example that will iterate 10 times:

```rs
let mut counter = 0;

loop counter < 9 {
  // .. code ..

  counter = counter + 1;
}
```

#### &mdash; Attributes

Attributes can be used to modify the behavior of functions and externs. They act as metadata, and only exist during compile-time. Below is the syntax for attributes:

```java
@attribute_name // no arguments, equivalent to `@attribute_name()`
@example_1(arguments)
```

Attribute names must be valid identifiers, and they may optionally contain an argument list. Having duplicate attributes attached to a single function will result in an error, as well as the use of an undefined/unrecognized attribute.

Below is a list of all the intrinsic attributes available:

- `@deprecated`: Marks a function as deprecated. A warning will be issued if the attached function is called.
- `@inline`: Marks a function as _inline_. This will cause the compiler to inline the function, which may result in a performance gain.
- `@export`: Marks a function to be exported externally. Its named will not be mangled.
- `@no_discard`: The result of the function must be used, otherwise a warning will be issued.
- `@tail_recursive`: Marks a recursive function to be validated for tail-recursion. In case that the function cannot be validated to be
  tail-recursive, an error will be issued. This is useful to prevent possible stack overflow exceptions caused by logic errors.
- `@calling_convention(str)`: Specifies the calling convention of an extern function. Attaching this attribute to a non-extern function will result in an error.

### Building

#### &mdash; Environment variables

**If building from source**: Set the `LLVM_SYS_120_PREFIX` environment variable to the `build` directory inside the LLVM source files. It is expected that LLVM was built from source at this point. Additionally, set the `LLVM_CONFIG` to point to the `build/bin/llvm-config` (or `build/bin/llvm-config.exe` on Windows) executable file. Do not wrap the path with quotes, as it might lead to `Access denied` errors when attempting to build `llvm-sys`. If you're using Visual Studio Code, ensure it is seeing the `LLVM_SYS_120_PREFIX` environment variable.

#### &mdash; Linux

On Linux, you simply need to install the `llvm` and `llvm-devel` packages. Make sure they're both `v13.0.0`.

If you're using Fedora:

```bash
$ sudo dnf -y install llvm llvm-devel
```

You won't need to set any environment variables. Additionally, avoid using the `llvmenv` crate, since it's barely maintained and may lead you to issues.

After installing, you can verify you're running the correct LLVM version by using the `llvm-config` command:

```bash
$ llvm-config --version
13.0.0
```

If, after installing `llvm-devel` and restarting your shell session you're still having problems with the `inkwell` crate not being able to find the LLVM installation, try setting the `LLVM_SYS_130_PREFIX` environment variable manually:

```bash
$ export LLVM_SYS_130_PREFIX=${llvm-config --prefix}
```

That should fix the problem, then try running `cargo build` once more.

#### &mdash; Windows

On the Windows platform, it is recommended to use MSYS2 to install the GCC toolchain. After installing MSYS2, open the `MSYS2 MinGW (64-bit)` console (or the `32-bit` if you're on 32-bit arch.), then install the GCC toolchain using:

```bash
$ pacman -S mingw-w64-x86_64-gcc
```

This project uses the `inkwell` crate to interface with LLVM. Thus, the GCC toolchain (through MSYS2) is required in order to build the `llvm-sys` Cargo package (which is a dependency of `inkwell`).

#### &mdash; Building Cargo crate

You will need to have [🔗Rust](https://www.rust-lang.org/tools/install) installed in order to build the project using Cargo. Once (or
if you already have it) installed, you can simply build the project and its dependencies by issuing the following command:

```bash
$ cargo build
```

Running tests is also straight-forward:

```bash
$ cargo test
```
