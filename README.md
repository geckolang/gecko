<p align="center">
  <img alt="Logo" width="200" src="https://i.ibb.co/fFtvn08/Gecko-Logo-Logo-Only-01.png" />
  <br/>
  <br/>
  <i>Gecko is a high-level, general-purpose programming language built on top of the LLVM project.</i>
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

Gecko is a general-purpose, strongly-typed programming language, with a focus on a powerful type system, memory safety, and simplicity. Built using Rust. It uses [🔗LLVM](https://llvm.org/) as its backend.

[🔗Join our Discord server](https://discord.gg/H3eMUXp)

Overview:

- [Syntax example](#syntax-example)
- [Project roadmap](#project-roadmap)
- [Directory structure](#directory-structure)
- [Language specification](#language-specification)
- [Building](#building)

#### Syntax example

```rust
struct Human {
  name: &str,
  age: unsigned i8
}

fn greet(Human human) {
  printf(
    "Greetings! My name is %s and I am %s years old.",
    human.name,
    human.age
  )
}

fn main(argc: i32, argv: i32[]) ~ i32 {
  let dwayneJohnson = Human {
    "Dwayne Johnson",
    49
  }

  greet(dwayneJohnson)

  return 0
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

#### 1.1 &mdash; Naming & name mangling

Naming is straight forward. Whitespace and most special characters are disallowed in names, however the following exceptions exist: `$`, `_`. Names must not start with a number. They must also not be reserved keywords or types.

Here is the exact regular expression rule for names:

```r
^([_a-zA-Z]+[\w]*)
```

[_🔗test this regular expression_](https://regex101.com/r/KDIWdL/1)

Name mangling affects functions, structs, and globals. Names of any entities defined on the global environment (under the lack of a module definition) are _not_ name mangled. In other words, only entities under module are affected by name mangling. Externs are never name mangled, even if declared under a module.

#### 1.2 &mdash; Comments

Only single-line comments are available for simplicity. All comments start with the `#` character, and anything after that is considered part of the comment and is ignored by the compiler.

```py
# This is a comment.
```

#### 1.3 &mdash; Types

Several intrinsic types are defined by the compiler. It is intended for the intrinsic types to be bare-bones, and to have the standard library expand upon them, this allows for easier refactoring of type-specific functions, without having to modify the compiler's source code.

| Definition | Description                                                                                                         |
| ---------- | ------------------------------------------------------------------------------------------------------------------- |
| `bool`     | Boolean type. Its value can either be `true` or `false`.                                                            |
| `i8`       | Integer type with bit-size 8. Can be used to define characters, as well as strings as a pointer.                    |
| `i16`      | Integer type with bit-size 16. Equivalent to a `short int` on other languages.                                      |
| `i32`      | Integer type with bit-size 32. Equivalent to an `int` on other languages. Usually the most common number type used. |
| `i64`      | Integer type with bit-size 64. Equivalent to a `long int` on other languages. Useful for larger numbers.            |

#### 1.4 &mdash; Modules

Modules provide a simple way of organizing code within a project. They also have the advantage of preventing global naming collisions (ex. when importing a library). Modules are based off the file name, and are to be defined in the `src/` directory. They are not declared in code for simplicity.

Accessing a module is trivial:

```rust
foo::bar::entity
```

#### 1.5 &mdash; Functions

Function definitions & calls follow conventional norms. They are easy to define and use. The language grammar was designed in a way to have only one way to achieve things, with the idea that limited options remove the problems of different programmers using different methods of accomplishing the same thing (ex. different function declaration syntaxes). This way, whenever you encounter code you know what to expect right away.

Omitting the return type will imply that such function does not return a value (the equivalent to other languages' `void` type). Functions without return types must not return a value, nor are they required to have a `return` statement.

```rust
fn main(argc: i32, argv: i32[]) ~ i32 {
  return 0;
}

fn do_nothing() { }
```

#### 1.6 &mdash; Variables

Variable declaration, assignment and reference follow straight-forward rules and adhere to common conventions. This makes creating, and using variables easy and most programmers will be familiar with this style. Variable names adhere to the `name` rule.

```rust
fn double_number(number: i32) ~ i32 {
  let result: i32 = number * 2;

  return result;
}
```

For convenience, variables can also be declared without specifying their types by using the `let` keyword for type inference. When inferring type from a literal integer, the preferred type inferred by the compiler will be `i32`, unless the integer cannot fit into `i32`'s bit-size, in which case it will be either `i64` or `i128` depending on the value's required bit-size. For example, a value larger than `2147483647` will be inferred as `i64` because it cannot fit into `i32`.

```rust
fn do_work() ~ i32 { return 1; }

fn do_computation() ~ i32 {
  let work = do_work(); # Inferred i32 type from function call.
  let nextWork = work + 1; # Inferred i32 type from expression (i32 + i32 = i32).
  let workConst = 7; # Inferred i32 type from literal.

  return work + nextWork + workConst;
}
```

#### 1.7 &mdash; Statements &amp; loops

The language includes support for conditional statements, variable statements, and loops.

```rust
fn do_work() {
  let mut number = 1;

  number = 2;

  if true { }
  else if false { }
  else { }

  loop { }

  while true { break }

  for i = 0; i < 10; i += 1 { }

  match true {
    true -> do_work(),
    false -> do_work(),
    _ -> do_work()
  }

  return;
}
```

#### 1.8 &mdash; Safety &amp; error handling

...

#### 1.9 &mdash; Generics

...

### Building

#### 1.1 &mdash; Environment variables

**If building from source**: Set the `LLVM_SYS_120_PREFIX` environment variable to the `build` directory inside the LLVM source files. It is expected that LLVM was built from source at this point. Additionally, set the `LLVM_CONFIG` to point to the `build/bin/llvm-config` (or `build/bin/llvm-config.exe` on Windows) executable file. Do not wrap the path with quotes, as it might lead to `Access denied` errors when attempting to build `llvm-sys`. If you're using Visual Studio Code, ensure it is seeing the `LLVM_SYS_120_PREFIX` environment variable.

#### 1.2 &mdash; Linux

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

#### 1.3 &mdash; Windows

On the Windows platform, it is recommended to use MSYS2 to install the GCC toolchain. After installing MSYS2, open the `MSYS2 MinGW (64-bit)` console (or the `32-bit` if you're on 32-bit arch.), then install the GCC toolchain using:

```bash
$ pacman -S mingw-w64-x86_64-gcc
```

This project uses the `inkwell` crate to interface with LLVM. Thus, the GCC toolchain (through MSYS2) is required in order to build the `llvm-sys` Cargo package (which is a dependency of `inkwell`).

#### 1.4 &mdash; Building Cargo crate

You will need to have [🔗Rust](https://www.rust-lang.org/tools/install) installed in order to build the project using Cargo. Once (or
if you already have it) installed, you can simply build the project and its dependencies by issuing the following command:

```bash
$ cargo build
```

Running tests is also straight-forward:

```bash
$ cargo test
```
