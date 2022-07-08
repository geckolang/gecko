; ModuleID = 'intrinsic'
source_filename = "intrinsic"

%.1.struct.Complex = type { i8*, i32 }

@string_literal = private unnamed_addr constant [8 x i8] c"literal\00", align 1
@string_literal.1 = private unnamed_addr constant [8 x i8] c"binding\00", align 1

define private void @.0.size_of() {
fn.entry:
  %var.int_size = alloca i64, align 8
  store i64 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i64), i64* %var.int_size, align 4
  %var.person_size = alloca i64, align 8
  store i64 ptrtoint (%.1.struct.Complex* getelementptr (%.1.struct.Complex, %.1.struct.Complex* null, i32 1) to i64), i64* %var.person_size, align 4
  ret void
}

define private void @.2.length_of() {
fn.entry:
  %array.value = alloca [3 x i32], align 4
  %array.init = getelementptr [3 x i32], [3 x i32]* %array.value, i32 0, i32 0
  store i32 1, i32* %array.init, align 4
  %array.init1 = getelementptr [3 x i32], [3 x i32]* %array.value, i32 0, i32 1
  store i32 2, i32* %array.init1, align 4
  %array.init2 = getelementptr [3 x i32], [3 x i32]* %array.value, i32 0, i32 2
  store i32 3, i32* %array.init2, align 4
  %access = load [3 x i32], [3 x i32]* %array.value, align 4
  %var.arr = alloca [3 x i32], align 4
  store [3 x i32] %access, [3 x i32]* %var.arr, align 4
  %access3 = load [3 x i32], [3 x i32]* %var.arr, align 4
  %var.arr_len = alloca i32, align 4
  store i32 3, i32* %var.arr_len, align 4
  ret void
}

define private void @.3.panic() {
fn.entry:
  %0 = call i32 @puts(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @string_literal, i32 0, i32 0))
  call void @intrinsic.panic()
  %var.msg = alloca i8*, align 8
  store i8* getelementptr inbounds ([8 x i8], [8 x i8]* @string_literal.1, i32 0, i32 0), i8** %var.msg, align 8
  %access = load i8*, i8** %var.msg, align 8
  %1 = call i32 @puts.2(i8* %access)
  call void @intrinsic.panic()
  unreachable
}

declare void @abort()

define void @intrinsic.panic() {
entry:
  call void @abort()
  unreachable
}

declare i32 @puts(i8*)

declare i32 @puts.2(i8*)
