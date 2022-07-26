define void @test() {
entry:
  %var.a = alloca i32, align 4
  store i32 1, i32* %var.a, align 4
  %access = load i32, i32* %var.a, align 4
  %var.b = alloca i32, align 4
  store i32 %access, i32* %var.b, align 4
}
