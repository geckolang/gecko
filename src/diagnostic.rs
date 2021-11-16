#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Severity {
  Warning,
  Error,
  Internal,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
  pub message: String,
  pub severity: Severity,
}

pub fn unreachable() -> Diagnostic {
  Diagnostic {
    message: "unreachable point reached".into(),
    severity: Severity::Internal,
  }
}
