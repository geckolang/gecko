#[macro_export]
// TODO: Replace this with an `expect` method (under `Lexer).
macro_rules! diagnostic_assert {
  ($condition:expr) => {
    match $condition {
      true => true,
      false => {
        return Err(diagnostic::Diagnostic {
          message: format!("assertion failed: `{}`", stringify!($condition)),
          severity: diagnostic::Severity::Internal,
          // TODO: No location.
          location: None,
        });
      }
    }
  };
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Severity {
  Warning,
  Error,
  Internal,
}

pub type Location = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq)]
pub struct Diagnostic {
  pub message: String,
  pub severity: Severity,
  pub location: Option<Location>,
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
      location: None,
    });
  }

  pub fn warning(&mut self, message: String) {
    self.diagnostics.push(Diagnostic {
      message,
      severity: Severity::Warning,
      location: None,
    });
  }
}

impl Into<Vec<Diagnostic>> for DiagnosticBuilder {
  fn into(self) -> Vec<Diagnostic> {
    self.diagnostics
  }
}
