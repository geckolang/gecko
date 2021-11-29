#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Severity {
  Warning,
  Error,
  Internal,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Diagnostic {
  pub message: String,
  pub severity: Severity,
}

impl Diagnostic {
  /// Determine whether the error is non-informational, and instead
  /// denotes a problem in either the program or the compiler itself.
  pub fn is_error_like(&self) -> bool {
    match self.severity {
      Severity::Error | Severity::Internal => true,
      _ => false,
    }
  }
}

pub fn unreachable() -> Diagnostic {
  Diagnostic {
    message: "unreachable point reached".to_string(),
    severity: Severity::Internal,
  }
}
