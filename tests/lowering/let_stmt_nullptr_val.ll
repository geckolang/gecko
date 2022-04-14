define void @test() {
entry:
  %var.a = alloca i32*, align 8
  store i32* null, i32** %var.a, align 8
}
