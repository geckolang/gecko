; ModuleID = 'test'
source_filename = "test"

define private i32 @.0.if_expr() {
fn.entry:
  br i1 true, label %if.then, label %if.after

if.then:                                          ; preds = %fn.entry
  ret i32 0

if.after:                                         ; preds = %fn.entry
  ret i32 1
}
