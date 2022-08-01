define void @test() {
entry:
  %var.a = alloca i32*, align 8
  store i32* null, i32** %var.a, align 8
  %access = load i32*, i32** %var.a, align 8
  %var.b = alloca i32*, align 8
  store i32* %access, i32** %var.b, align 8
}
