; ModuleID = 'type'
source_filename = "type"

define private i32 @.0.unit_binding() {
fn.entry:
  br i1 true, label %if.then, label %if.after

if.then:                                          ; preds = %fn.entry
  br label %if.after

if.after:                                         ; preds = %if.then, %fn.entry
  br i1 true, label %if.then1, label %if.after2

if.then1:                                         ; preds = %if.after
  ret i32 1

if.after2:                                        ; preds = %if.after
  ret i32 0
}

define private i32 @.1.all_paths_covered() {
fn.entry:
  br i1 true, label %if.then, label %if.else

if.then:                                          ; preds = %fn.entry
  ret i32 0

if.after:                                         ; No predecessors!
  unreachable

if.else:                                          ; preds = %fn.entry
  ret i32 1
}

define private i32 @.2.never() {
fn.entry:
  br i1 true, label %if.then, label %if.else

if.then:                                          ; preds = %fn.entry
  ret i32 1

if.after:                                         ; No predecessors!
  ret i32 0

if.else:                                          ; preds = %fn.entry
  ret i32 2
}
