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
