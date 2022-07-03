; ModuleID = 'test'
source_filename = "test"

define private i32 @.0.blocks() {
fn.entry:
  %var.a = alloca i32, align 4
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.else

if.then:                                          ; preds = %fn.entry
  store i32 1, i32* %if.value, align 4
  br label %if.after

if.after:                                         ; preds = %if.then, %if.else
  %access = load i32, i32* %if.value, align 4
  store i32 %access, i32* %var.a, align 4
  %var.b = alloca i32, align 4
  %if.value1 = alloca i32, align 4
  br i1 true, label %if.then2, label %if.else4

if.else:                                          ; preds = %fn.entry
  store i32 2, i32* %if.value, align 4
  br label %if.after

if.then2:                                         ; preds = %if.after
  store i32 1, i32* %if.value1, align 4
  br label %if.after3

if.after3:                                        ; preds = %if.then2, %if.else4
  %access5 = load i32, i32* %if.value1, align 4
  store i32 %access5, i32* %var.b, align 4
  %access6 = load i32, i32* %var.a, align 4
  %access7 = load i32, i32* %var.b, align 4
  %int.add_op = add i32 %access6, %access7
  ret i32 %int.add_op

if.else4:                                         ; preds = %if.after
  store i32 2, i32* %if.value1, align 4
  br label %if.after3
}

define private void @.1.pass_keyword() {
fn.entry:
  ret void
}
