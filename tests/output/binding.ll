; ModuleID = 'binding'
source_filename = "binding"

@panic.msg = private unnamed_addr constant [21 x i8] c"panic: unimplemented\00", align 1

define private void @.0.binding() {
fn.entry:
  %var.a = alloca i32, align 4
  store i32 1, i32* %var.a, align 4
  %var.b = alloca i32, align 4
  store i32 2, i32* %var.b, align 4
  %access = load i32, i32* %var.a, align 4
  %access1 = load i32, i32* %var.b, align 4
  %int.add_op = add i32 %access, %access1
  %var.c = alloca i32, align 4
  store i32 %int.add_op, i32* %var.c, align 4
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.else

if.then:                                          ; preds = %fn.entry
  %access3 = load i32, i32* %var.a, align 4
  store i32 %access3, i32* %if.value, align 4
  br label %if.after

if.after:                                         ; preds = %if.then, %if.else
  %access4 = load i32, i32* %if.value, align 4
  %var.d = alloca i32, align 4
  store i32 %access4, i32* %var.d, align 4
  %0 = call i32 @puts(i8* getelementptr inbounds ([21 x i8], [21 x i8]* @panic.msg, i32 0, i32 0))
  call void @intrinsic.panic()
  %var.g = alloca i32, align 4
  store i32 1, i32* %var.g, align 4
  %var.h = alloca i32, align 4
  store i32 2, i32* %var.h, align 4
  store i32 3, i32* %var.h, align 4
  unreachable

if.else:                                          ; preds = %fn.entry
  %access2 = load i32, i32* %var.b, align 4
  store i32 %access2, i32* %if.value, align 4
  br label %if.after
}

define private i32 @.1.closure() {
closure.entry:
  ret i32 0
}

declare i32 @puts(i8*)

declare void @abort()

define void @intrinsic.panic() {
entry:
  call void @abort()
  unreachable
}
