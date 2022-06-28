; ModuleID = 'test'
source_filename = "test"

%.1.struct.A = type { i32 }

define private i32 @.0.let_stmt_struct_value() {
fn.entry:
  %var.a = alloca %.1.struct.A, align 8
  %struct.A.alloca = alloca %.1.struct.A, align 8
  %struct.alloca.field.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %struct.A.alloca, i32 0, i32 0
  store i32 1, i32* %struct.alloca.field.gep, align 4
  %access = load %.1.struct.A, %.1.struct.A* %struct.A.alloca, align 4
  store %.1.struct.A %access, %.1.struct.A* %var.a, align 4
  %struct.member.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %var.a, i32 0, i32 0
  %access1 = load i32, i32* %struct.member.gep, align 4
  ret i32 %access1
}

define private i32 @.2.inline_value() {
fn.entry:
  %var.b = alloca i32, align 4
  %struct.A.alloca = alloca %.1.struct.A, align 8
  %struct.alloca.field.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %struct.A.alloca, i32 0, i32 0
  store i32 2, i32* %struct.alloca.field.gep, align 4
  %struct.member.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %struct.A.alloca, i32 0, i32 0
  %access = load i32, i32* %struct.member.gep, align 4
  store i32 %access, i32* %var.b, align 4
  %access1 = load i32, i32* %var.b, align 4
  ret i32 %access1
}
