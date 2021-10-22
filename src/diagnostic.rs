#[derive(Clone)]
pub enum DiagnosticSeverity {
  Warning,
  Error,
  Internal,
}

#[derive(Clone)]
pub struct Diagnostic {
  pub message: String,
  pub severity: DiagnosticSeverity,
}
