; ModuleID = 'test'
source_filename = "test"

define private i32 @.0.if_expr() {
fn.entry:
  br i1 true, label %if.then, label %if.after

if.then:                                          ; preds = %fn.entry
  ret i32 0

if.after:                                         ; preds = %fn.entry
  br i1 true, label %if.then1, label %if.else

if.then1:                                         ; preds = %if.after
  ret i32 1

if.after2:                                        ; No predecessors!
  br i1 true, label %if.then3, label %if.else5

if.else:                                          ; preds = %if.after
  ret i32 2

if.then3:                                         ; preds = %if.after2
  ret i32 3

if.after4:                                        ; No predecessors!
  ret i32 7

if.else5:                                         ; preds = %if.after2
  ret i32 6
}
