pub struct DefinitionKey(pub usize);

pub struct Definition {
  pub name: String,
}

pub struct Context {
  pub definitions: Vec<Definition>,
}

impl Context {
  pub fn push_definition(&mut self, name: String) -> DefinitionKey {
    let key = DefinitionKey(self.definitions.len());

    self.definitions.push(Definition { name });

    key
  }
}
