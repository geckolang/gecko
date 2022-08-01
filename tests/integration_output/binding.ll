; ModuleID = 'binding'
source_filename = "binding"

define private void @.0.binding() {
fn.entry:
  %var.a = alloca i32, align 4
  store i32 1, i32* %var.a, align 4
  %var.b = alloca i32, align 4
  store i32 2, i32* %var.b, align 4
  %var.a1 = alloca i32, align 4
  store i32 1, i32* %var.a1, align 4
  %access = load i32, i32* %var.a1, align 4
  %var.b2 = alloca i32, align 4
  store i32 2, i32* %var.b2, align 4
  %access3 = load i32, i32* %var.b2, align 4
  %int.add_op = add i32 %access, %access3
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.else

if.then:                                          ; preds = %fn.entry
  %var.a6 = alloca i32, align 4
  store i32 1, i32* %var.a6, align 4
  %access7 = load i32, i32* %var.a6, align 4
  store i32 %access7, i32* %if.value, align 4
  br label %if.after

if.after:                                         ; preds = %if.then, %if.else
  %access8 = load i32, i32* %if.value, align 4
  %var.d = alloca i32, align 4
  store i32 %access8, i32* %var.d, align 4
  %var.g = alloca i32, align 4
  store i32 1, i32* %var.g, align 4
  unreachable

if.else:                                          ; preds = %fn.entry
  %var.b4 = alloca i32, align 4
  store i32 2, i32* %var.b4, align 4
  %access5 = load i32, i32* %var.b4, align 4
  store i32 %access5, i32* %if.value, align 4
  br label %if.after
}
