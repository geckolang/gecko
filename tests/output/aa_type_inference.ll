; ModuleID = 'aa_type_inference'
source_filename = "aa_type_inference"

define private i1 @.0.test(i1 %param.a) {
fn.entry:
  %array.value = alloca [1 x i32], align 4
  %array.init = getelementptr [1 x i32], [1 x i32]* %array.value, i32 0, i32 0
  store i32 1, i32* %array.init, align 4
  %access = load [1 x i32], [1 x i32]* %array.value, align 4
  %var.arr = alloca [1 x i32], align 4
  store [1 x i32] %access, [1 x i32]* %var.arr, align 4
  %array.value1 = alloca [0 x i32], align 4
  %access2 = load [0 x i32], [0 x i32]* %array.value1, align 4
  %var.arr2 = alloca [0 x i32], align 4
  store [0 x i32] %access2, [0 x i32]* %var.arr2, align 4
  %not_op = xor i1 %param.a, true
  ret i1 %not_op
}
