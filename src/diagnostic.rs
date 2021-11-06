#[derive(Clone, Debug, Eq, PartialEq)]
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

impl std::fmt::Display for Diagnostic {
  fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use colored::*;

    let severity_string = match self.severity {
      DiagnosticSeverity::Warning => "warning".yellow(),
      DiagnosticSeverity::Error => "error".red(),
      DiagnosticSeverity::Internal => "internal compiler error".black().on_white(),
    };

    write!(formatter, "{}: {}", severity_string, self.message)
  }
}

pub fn error_unexpected_eof(expected: &str) -> Diagnostic {
  Diagnostic {
    message: format!("unexpected end of file, expected {}", expected),
    severity: DiagnosticSeverity::Error,
  }
}
