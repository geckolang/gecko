; ModuleID = 'externs'
source_filename = "externs"

@string_literal = private unnamed_addr constant [12 x i8] c"hello world\00", align 1

declare i32 @printf(i8*, ...)

define private void @.0.calling_externs() {
fn.entry:
  %call = call i32 (i8*, ...) @printf.1(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @string_literal, i32 0, i32 0))
  ret void
}

declare i32 @printf.1(i8*, ...)
