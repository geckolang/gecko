define void @test() {
entry:
  br i1 true, label %if.then, label %if.after
if.then: ; preds = %entry
  br label %if.after
if.after: ; preds = %if.then, %entry
}
