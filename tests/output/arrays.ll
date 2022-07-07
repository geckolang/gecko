; ModuleID = 'arrays'
source_filename = "arrays"

@panic.msg = private unnamed_addr constant [33 x i8] c"panic: array index out of bounds\00", align 1
@panic.msg.1 = private unnamed_addr constant [33 x i8] c"panic: array index out of bounds\00", align 1

define private i32 @.0.arrays() {
fn.entry:
  %array.value = alloca [0 x i32], align 4
  %access = load [0 x i32], [0 x i32]* %array.value, align 4
  %var.empty_arr = alloca [0 x i32], align 4
  store [0 x i32] %access, [0 x i32]* %var.empty_arr, align 4
  %array.value1 = alloca [1 x i32], align 4
  %array.init = getelementptr [1 x i32], [1 x i32]* %array.value1, i32 0, i32 0
  store i32 1, i32* %array.init, align 4
  %access2 = load [1 x i32], [1 x i32]* %array.value1, align 4
  %var.arr = alloca [1 x i32], align 4
  store [1 x i32] %access2, [1 x i32]* %var.arr, align 4
  %array.index.gep = getelementptr inbounds [1 x i32], [1 x i32]* %var.arr, i32 0, i32 0
  br i1 true, label %panic.next, label %panic

panic.next:                                       ; preds = %fn.entry
  %access3 = load i32, i32* %array.index.gep, align 4
  %array.value4 = alloca [1 x i32], align 4
  %array.init5 = getelementptr [1 x i32], [1 x i32]* %array.value4, i32 0, i32 0
  store i32 %access3, i32* %array.init5, align 4
  %access6 = load [1 x i32], [1 x i32]* %array.value4, align 4
  %var.arr_2 = alloca [1 x i32], align 4
  store [1 x i32] %access6, [1 x i32]* %var.arr_2, align 4
  %array.index.gep7 = getelementptr inbounds [1 x i32], [1 x i32]* %var.arr, i32 0, i32 0
  br i1 true, label %panic.next8, label %panic9

panic:                                            ; preds = %fn.entry
  %0 = call i32 @puts(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @panic.msg, i32 0, i32 0))
  call void @intrinsic.panic()
  unreachable

panic.next8:                                      ; preds = %panic.next
  %access10 = load i32, i32* %array.index.gep7, align 4
  ret i32 %access10

panic9:                                           ; preds = %panic.next
  %1 = call i32 @puts.2(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @panic.msg.1, i32 0, i32 0))
  call void @intrinsic.panic()
  unreachable
}

declare i32 @puts(i8*)

declare void @abort()

define void @intrinsic.panic() {
entry:
  call void @abort()
  unreachable
}

declare i32 @puts.2(i8*)
