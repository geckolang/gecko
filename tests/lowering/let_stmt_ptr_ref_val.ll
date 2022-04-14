define void @test() {
entry:
  %var.b = alloca i32*, align 8
  %var.a = alloca i32*, align 8 ; Skipping the access for some reason.
  store i32* null, i32** %var.a, align 8
  store i32* %var.a, i32** %var.b, align 8 ; WRONG! Trying to store `i32**` into `i32**`.
}
