; ModuleID = 'intrinsic'
source_filename = "intrinsic"

%.1.struct.Complex = type { i8*, i32 }

@string_literal = private unnamed_addr constant [8 x i8] c"literal\00", align 1
@string_literal.1 = private unnamed_addr constant [8 x i8] c"binding\00", align 1

define private void @.0.sizeof_() {
fn.entry:
  %var.int_size = alloca i64, align 8
  store i64 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i64), i64* %var.int_size, align 4
  %var.person_size = alloca i64, align 8
  store i64 ptrtoint (%.1.struct.Complex* getelementptr (%.1.struct.Complex, %.1.struct.Complex* null, i32 1) to i64), i64* %var.person_size, align 4
  ret void
}

define private void @.2.panic() {
fn.entry:
  %0 = call i32 @puts(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @string_literal, i32 0, i32 0))
  call void @intrinsic.panic()
  %var.msg = alloca i8*, align 8
  store i8* getelementptr inbounds ([8 x i8], [8 x i8]* @string_literal.1, i32 0, i32 0), i8** %var.msg, align 8
  %access = load i8*, i8** %var.msg, align 8
  %1 = call i32 @puts.2(i8* %access)
  call void @intrinsic.panic()
  ret void
}

declare void @abort()

define void @intrinsic.panic() {
entry:
  call void @abort()
  unreachable
}

declare i32 @puts(i8*)

declare i32 @puts.2(i8*)
