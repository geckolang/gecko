; ModuleID = 'aa_type_inference'
source_filename = "aa_type_inference"

define private i1 @.0.test() {
fn.entry:
  %array.value = alloca [1 x i32], align 4
  %array.init = getelementptr [1 x i32], [1 x i32]* %array.value, i32 0, i32 0
  store i32 1, i32* %array.init, align 4
  %access = load [1 x i32], [1 x i32]* %array.value, align 4
  ret i1 true
}
