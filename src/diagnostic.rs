#[derive(Clone, Debug)]
pub enum DiagnosticSeverity {
  Warning,
  Error,
  Internal,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
  pub message: String,
  pub severity: DiagnosticSeverity,
}

pub fn error_unexpected_eof(expected: &str) -> Diagnostic {
  Diagnostic {
    message: format!("unexpected end of file, expected {}", expected),
    severity: DiagnosticSeverity::Error,
  }
}
