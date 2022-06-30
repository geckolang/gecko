; ModuleID = 'test'
source_filename = "test"

@string_literal = private unnamed_addr constant [2 x i8] c"a\00", align 1
@string_literal.1 = private unnamed_addr constant [2 x i8] c"b\00", align 1

define private i8* @.0.reflect_string(i8* %param.string) {
fn.entry:
  ret i8* %param.string
}

define private void @.1.strings() {
fn.entry:
  %var.a = alloca i8*, align 8
  store i8* getelementptr inbounds ([2 x i8], [2 x i8]* @string_literal, i32 0, i32 0), i8** %var.a, align 8
  %var.b = alloca i8*, align 8
  %access = load i8*, i8** %var.a, align 8
  store i8* %access, i8** %var.b, align 8
  %var.c = alloca i8*, align 8
  %access1 = load i8*, i8** %var.b, align 8
  %call = call i8* @.0.reflect_string(i8* %access1)
  store i8* %call, i8** %var.c, align 8
  %var.d = alloca i8*, align 8
  %call2 = call i8* @.0.reflect_string(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @string_literal.1, i32 0, i32 0))
  store i8* %call2, i8** %var.d, align 8
  ret void
}
