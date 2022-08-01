; ModuleID = 'struct'
source_filename = "struct"

%.1.struct.A = type { i32 }

define private i32 @.0.binding_stmt_struct_value() {
fn.entry:
  %struct.A.alloca = alloca %.1.struct.A, align 8
  %struct.alloca.field.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %struct.A.alloca, i32 0, i32 0
  store i32 1, i32* %struct.alloca.field.gep, align 4
  %access = load %.1.struct.A, %.1.struct.A* %struct.A.alloca, align 4
  %var.a = alloca %.1.struct.A, align 8
  store %.1.struct.A %access, %.1.struct.A* %var.a, align 4
  %struct.member.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %var.a, i32 0, i32 0
  %access1 = load i32, i32* %struct.member.gep, align 4
  ret i32 %access1
}

define private i32 @.2.inline_value() {
fn.entry:
  %struct.A.alloca = alloca %.1.struct.A, align 8
  %struct.alloca.field.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %struct.A.alloca, i32 0, i32 0
  store i32 2, i32* %struct.alloca.field.gep, align 4
  %struct.member.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %struct.A.alloca, i32 0, i32 0
  %access = load i32, i32* %struct.member.gep, align 4
  %var.b = alloca i32, align 4
  store i32 %access, i32* %var.b, align 4
  %access1 = load i32, i32* %var.b, align 4
  ret i32 %access1
}

define private i32 @.3.impl_method_call() {
fn.entry:
  %struct.A.alloca = alloca %.1.struct.A, align 8
  %struct.alloca.field.gep = getelementptr inbounds %.1.struct.A, %.1.struct.A* %struct.A.alloca, i32 0, i32 0
  store i32 3, i32* %struct.alloca.field.gep, align 4
  %access = load %.1.struct.A, %.1.struct.A* %struct.A.alloca, align 4
  %var.c = alloca %.1.struct.A, align 8
  store %.1.struct.A %access, %.1.struct.A* %var.c, align 4
  %call = call i32 @.4.method(%.1.struct.A* %var.c)
  %var.d = alloca i32, align 4
  store i32 %call, i32* %var.d, align 4
  %access1 = load i32, i32* %var.d, align 4
  ret i32 %access1
}

define private i32 @.4.method(%.1.struct.A* %0) {
fn.entry:
  ret i32 1
}

define private i32 @.5.method(%.1.struct.A* %0) {
fn.entry:
  ret i32 1
}
