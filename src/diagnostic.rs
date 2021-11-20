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

impl Diagnostic {
  pub fn is_error_like(&self) -> bool {
    self.severity == Severity::Error || self.severity == Severity::Internal
  }
}

pub fn unreachable() -> Diagnostic {
  Diagnostic {
    message: "unreachable point reached".into(),
    severity: Severity::Internal,
  }
}
