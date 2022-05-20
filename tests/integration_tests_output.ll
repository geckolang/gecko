; ModuleID = 'test'
source_filename = "test"

define private i32 @.0.let_test() {
fn.entry:
  %var.a = alloca i32, align 4
  store i32 1, i32* %var.a, align 4
  %var.b = alloca i32, align 4
  store i32 2, i32* %var.b, align 4
  %var.c = alloca i32, align 4
  %access = load i32, i32* %var.a, align 4
  %access1 = load i32, i32* %var.b, align 4
  %int.add_op = add i32 %access, %access1
  store i32 %int.add_op, i32* %var.c, align 4
  ret i32 0
}

define private i32 @.1.single_statement() {
fn.entry:
  ret i32 1
}

define private i32 @.2.block_yield() {
fn.entry:
  ret i32 1
}

define private i32 @.3.unsafe_block() {
fn.entry:
  ret i32 1
}

define i32 @main() {
fn.entry:
  ret i32 0
}

define private i32 @.4.struct_test() {
fn.entry:
  ret i32 0
}
