; ModuleID = 'let'
source_filename = "let"

define private void @.0.let_test() {
fn.entry:
  %var.a = alloca i32, align 4
  store i32 1, i32* %var.a, align 4
  ret void
}
