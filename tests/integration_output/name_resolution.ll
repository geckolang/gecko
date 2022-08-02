; ModuleID = 'name_resolution'
source_filename = "name_resolution"

define private i1 @.0.first(i1 %param.a) {
fn.entry:
  %not_op = xor i1 %param.a, true
  ret i1 %not_op
}

define private i32 @.1.second(i32 %param.a) {
fn.entry:
  %var.b = alloca i32, align 4
  store i32 1, i32* %var.b, align 4
  %access = load i32, i32* %var.b, align 4
  %var.c = alloca i32, align 4
  store i32 %access, i32* %var.c, align 4
  %int.add_op = add i32 %param.a, %param.a
  ret i32 %int.add_op
}
