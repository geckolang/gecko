define void @test() {
entry:
  %var.a = alloca i8*, align 8
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @string_literal, i32 0, i32 0), i8** %var.a, align 8
}
