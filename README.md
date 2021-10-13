#### ionlang
ionlang project written in Rust.

### Building
#### 1.1 &mdash; Environment variables
Set the `LLVM_SYS_120_PREFIX` environment variable to the `build` directory inside the LLVM source files. It is expected that LLVM was built from source at this point. Additionally, set the `LLVM_CONFIG` to point to the `build/bin/llvm-config` (or `build/bin/llvm-config.exe` on Windows) executable file.
