use crate::node;

// TODO:
pub fn resolve_callee_stub<'a>(callee_stub: &mut node::CalleeStub<'a>) {
  callee_stub.value = None;
}
