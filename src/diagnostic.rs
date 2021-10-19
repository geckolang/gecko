pub enum DiagnosticSeverity {
  Warning,
  Error,
  Internal,
}

pub struct Diagnostic {
  pub message: String,
  pub severity: DiagnosticSeverity,
}
