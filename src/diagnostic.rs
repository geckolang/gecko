#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Severity {
  Warning,
  Error,
  Internal,
}

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq)]
pub struct Diagnostic {
  pub message: String,
  pub severity: Severity,
  pub span: Option<Span>,
}

impl Diagnostic {
  /// Determine whether the error is non-informational, and instead
  /// denotes a problem in either the program or the compiler itself.
  pub fn is_error_like(&self) -> bool {
    matches!(self.severity, Severity::Error | Severity::Internal)
  }
}

pub struct DiagnosticBuilder {
  pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBuilder {
  pub fn new() -> Self {
    Self {
      diagnostics: Vec::new(),
    }
  }

  pub fn error(&mut self, message: String) {
    self.diagnostics.push(Diagnostic {
      message,
      severity: Severity::Error,
      span: None,
    });
  }

  pub fn warning(&mut self, message: String) {
    self.diagnostics.push(Diagnostic {
      message,
      severity: Severity::Warning,
      span: None,
    });
  }
}

impl Into<Vec<Diagnostic>> for DiagnosticBuilder {
  fn into(self) -> Vec<Diagnostic> {
    self.diagnostics
  }
}
