; ModuleID = 'test'
source_filename = "test"

define private void @.0.closure() {
fn.entry:
  %var.doubled = alloca i32, align 4
  %call = call i32 @.1.closure(i32 2)
  store i32 %call, i32* %var.doubled, align 4
  ret void
}

define private i32 @.1.closure(i32 %0) {
closure.entry:
  %int.multiply_op = mul i32 %0, 2
  ret i32 %int.multiply_op
}
