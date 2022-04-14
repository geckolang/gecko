define void @test() {
entry:
  br i1 true, label %if.then, label %if.after
if.then:
  br label %if.after
if.after:
}
