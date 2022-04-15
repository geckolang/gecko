define void @test() {
entry:
  %var.a = alloca i32, align 4
  store i32 1, i32* %var.a, align 4
}
