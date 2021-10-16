<p align="center">
  <img alt="Ion banner graphic" src="https://repository-images.githubusercontent.com/233767923/b799f780-d9cf-11ea-9d70-2597e4821c9e" />
</p>

#### ionlang
Ion is a general purpose, strongly-typed programming language, with a focus on a powerful type system, and simplicity. It uses `libLLVM` as its backend, and consumes the `libionir` & `libionshared` libraries. This project serves as the main library for consumption of both the compiler's frontend (lexing, parsing) & backend (lowering).

Thanks to `libLLVM`, compiled code is optimized to produce efficient programs.

[🔗Join our Discord server](https://discord.gg/H3eMUXp)

#### Syntax example
```rust
extern printf(i8* format, ...) -> i32;

struct Human {
  i8* name;

  ui8 age;
}

fn greet(Human human) {
  printf(
    "Greetings! My name is %s and I am %s years old.",
    human.name,
    human.age
  )
}

fn main(i32 argc, i32[] argv) -> i32 {
  let dwayneJohnson = Human{
    "Dwayne Johnson",
    49
  };

  greet(dwayneJohnson);

  return 0;
}
```

### Building
#### 1.1 &mdash; Environment variables
Set the `LLVM_SYS_120_PREFIX` environment variable to the `build` directory inside the LLVM source files. It is expected that LLVM was built from source at this point. Additionally, set the `LLVM_CONFIG` to point to the `build/bin/llvm-config` (or `build/bin/llvm-config.exe` on Windows) executable file.

### Project roadmap
*🔨 &mdash; Work in progress.* *✔️ &mdash; Completed.*
| Feature            | Note(s)                                                    | Status |
|--------------------|------------------------------------------------------------|--------|
| Functions          | -                                                          | ✔️      |
| Externs            | -                                                          | ✔️      |
| Function calls     | -                                                          | ✔️      |
| Structs            | Definition, declaration & accessing of structs.            | 🔨      |
| Global variables   | -                                                          | ✔️      |
| Namespaces         | -                                                          | ✔️      |
| Generics           | -                                                          | 🔨      |
| `if` statement     | Includes the `else` statement as well.                     | 🔨      |
| `return` statement | -                                                          | 🔨      |
| Variables          | Declaration, assignment, and reference of variables.       | 🔨      |
| Casting            | -                                                          | 🔨      |
| Binary expressions | -                                                          | ✔️      |
| Literals           | Includes string, integer, character, and boolean literals. | 🔨      |
| Types              | Intrinsic types such as `bool`, `i32`, `void`, etc.        | ✔️      |
| Arrays             | -                                                          | 🔨      |

### Directory structure
| Path             | Description                                                                                           |
|------------------|-------------------------------------------------------------------------------------------------------|
| `.github/`       | Contains GitHub configuration files. For example, the GitHub actions workflow file `build_cmake.yml`. |
| `include/`       | The root directory for the project's header files.                                                    |
| `libs/`          | Contains GitHub submodules used by the project.                                                       |
| `src/`           | The root directory for the project's source files.                                                    |
| `test/`          | Contains the project's test sub-project. Google tests is used.                                        |
| `.gitignore`     | Configuration file to specify paths ignored by Git.                                                   |
| `.gitmodules`    | Configuration file to specify the Git submodules used by the project.                                 |
| `CMakeLists.txt` | The CMake project file.                                                                               |
| `LICENSE`        | The license file.                                                                                     |
| `README.md`      | Information about the project.                                                                        |

### Language specification
#### 1.1 &mdash; Naming & name mangling
Naming is straight forward. Whitespace and most special characters are disallowed in names, however the following exceptions exist: `$`, `_`. Names must not start with a number. They must also not be reserved keywords or types.

Here is the exact regular expression rule for names:

```
^([_a-zA-Z]+[\w]*)
```
[*🔗test this regular expression*](https://regex101.com/r/KDIWdL/1)

Name mangling affects functions, structs, and globals. Names of any entities defined on the global environment (under the lack of a namespace definition) are *not* name mangled. In other words, only entities under namespaces are affected by name mangling. Externs are never name mangled, even if declared under a namespace.

#### 1.2 &mdash; Comments
Only single-line comments are available for simplicity. All comments start with the `#` character, and anything after that is considered part of the comment and is ignored by the compiler.

```py
# This is a comment.
```

#### 1.3 &mdash; Types
Several intrinsic types are defined by the compiler. It is intended for the intrinsic types to be bare-bones, and to have the standard library expand upon them, this allows for easier refactoring of type-specific functions, without having to modify the compiler's source code.

| Definition | Description                                                                                                         |
|------------|---------------------------------------------------------------------------------------------------------------------|
| `void`     | -                                                                                                                   |
| `bool`     | Boolean type. Its value can either be `true` or `false`.                                                            |
| `i8`       | Integer type with bit-size 8. Can be used to define characters, as well as strings as a pointer.                    |
| `i16`      | Integer type with bit-size 16. Equivalent to a `short int` on other languages.                                      |
| `i32`      | Integer type with bit-size 32. Equivalent to an `int` on other languages. Usually the most common number type used. |
| `i64`      | Integer type with bit-size 64. Equivalent to a `long int` on other languages. Useful for larger numbers.            |
| `i128`     | Integer type with bit-size 128. Equivalent to a `long long int` on other languages. Useful for larger numbers.      |

#### 1.4 &mdash; Namespaces
Namespaces provide a simple way of organizing code within a project. They also have the advantage of preventing global naming collisions (ex. when importing a library).

```cpp
namespace foo { }
```

Namespaces can be nested by separating their names with the `::` delimiter as follows:

```cpp
namespace foo::bar { }
```

Accessing a namespace is trivial:

```rust
foo::bar::entity;
```

#### 1.5 &mdash; Functions
Function definitions & calls follow conventional norms. They are easy to define and use. The language grammar was designed in a way to have only one way to achieve things, with the idea that limited options remove the problems of different programmers using different methods of accomplishing the same thing (ex. different function declaration syntaxes). This way, whenever you encounter code you know what to expect right away.

The return type of functions must always be specified, regardless of whether the function returns `void` or not. Functions with the `void` return type are not required to include a `return` statement.

```rust
fn main(i32 argc, i32[] argv) -> i32 {
  return 0;
}
```

#### 1.6 &mdash; Variables
Variable declaration, assignment and reference follow straight-forward rules and adhere to common conventions. This makes creating, and using variables easy and most programmers will be familiar with this style. Variable names adhere to the `name` rule.

```rust
fn double(i32 number) -> i32 {
  i32 doubledNumber = number * 2;

  return doubledNumber;
}
```

For convenience, variables can also be declared without specifying their types by using the `let` keyword for type inference. When inferring type from a literal integer, the preferred type inferred by the compiler will be `i32`, unless the integer cannot fit into `i32`'s bit-size, in which case it will be either `i64` or `i128` depending on the value's required bit-size. For example, a value larger than `2147483647` will be inferred as `i64` because it cannot fit into `i32`.

```rust
fn doWork() -> i32 { ... }

fn computeWork() -> i32 {
  let work = doWork(); # Inferred i32 type from function call.
  let nextWork = work + 1; # Inferred i32 type from expression (i32 + i32 = i32).
  let workConst = 7; # Inferred i32 type from literal.

  return work + nextWork + workConst;
}
```

#### 1.7 &mdash; Statements &amp; loops
The language includes support for conditional statements, variable statements, and loops.

```rust
fn doWork() -> void {
  let numberA = 1;
  i32 numberB = numberA;

  numberB = 2;

  if (true) { }
  else if (false) { }
  else { }

  while (true) { break; }

  do { } while (true);

  for (i32 i = 0; i < 10; i += 1) { }

  match (true) {
    true -> doWork(),
    false -> doWork(),
    _ -> doWork()
  }

  return;
}
```

#### 1.8 &mdash; Safety &amp; error handling
...

#### 1.9 &mdash; Generics
...
