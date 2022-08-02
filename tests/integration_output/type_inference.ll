; ModuleID = 'type_inference'
source_filename = "type_inference"

define private i1 @.0.infer_return_type_1(i1 %param.a) {
fn.entry:
  %not_op = xor i1 %param.a, true
  ret i1 %not_op
}

define private i32 @.1.infer_return_type_2(i32 %param.a) {
fn.entry:
  %int.add_op = add i32 %param.a, %param.a
  ret i32 %int.add_op
}

define private i32 @.2.infer_return_type_3(i32 %param.a) {
fn.entry:
  %int.add_op = add i32 %param.a, 1
  ret i32 %int.add_op
}

define private i1 @.3.test(i1 %param.a) {
fn.entry:
  %array.value = alloca [1 x i32], align 4
  %array.init = getelementptr [1 x i32], [1 x i32]* %array.value, i32 0, i32 0
  store i32 1, i32* %array.init, align 4
  %access = load [1 x i32], [1 x i32]* %array.value, align 4
  %var.array = alloca [1 x i32], align 4
  store [1 x i32] %access, [1 x i32]* %var.array, align 4
  %array.value1 = alloca [0 x i32], align 4
  %access2 = load [0 x i32], [0 x i32]* %array.value1, align 4
  %var.empty_array = alloca [0 x i32], align 4
  store [0 x i32] %access2, [0 x i32]* %var.empty_array, align 4
  %if.value = alloca i1, align 1
  br i1 true, label %if.then, label %if.else

if.then:                                          ; preds = %fn.entry
  store i1 true, i1* %if.value, align 1
  br label %if.after

if.after:                                         ; preds = %if.then, %if.else
  %access3 = load i1, i1* %if.value, align 1
  %var.b = alloca i1, align 1
  store i1 %access3, i1* %var.b, align 1
  %var.c = alloca i1, align 1
  store i1 false, i1* %var.c, align 1
  %access4 = load i1, i1* %var.b, align 1
  %and_op = and i1 %param.a, %access4
  %access5 = load i1, i1* %var.c, align 1
  %and_op6 = and i1 %and_op, %access5
  ret i1 %and_op6

if.else:                                          ; preds = %fn.entry
  store i1 false, i1* %if.value, align 1
  br label %if.after
}
