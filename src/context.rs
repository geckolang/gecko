pub type DefinitionKey = usize;

pub struct DefinitionInfo {
  pub name: String,
}

pub struct Context {
  pub definition_infos: Vec<DefinitionInfo>,
}

impl Context {
  pub fn push_definition(&mut self, name: String) -> DefinitionKey {
    let key = self.definition_infos.len();

    self.definition_infos.push(DefinitionInfo { name });

    key
  }

  pub fn is_memoized(&self, key: &DefinitionKey) -> bool {
    // TODO: Key being cloned. If nothing can be done, then remove the reference in the parameter?
    self.definition_infos.len() > *key
  }
}
