; ModuleID = 'test'
source_filename = "test"

%.1.struct.A = type { i32 }

define private i32 @.0.structs() {
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
