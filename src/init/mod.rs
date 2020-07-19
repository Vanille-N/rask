use crate::exec::Envt;
use chainmap::ChainMap;

mod operators;
mod type_identification;

pub trait Alias {
    fn alias(&mut self, s: &str, a: &str);
}

impl Alias for Envt {
    fn alias(&mut self, s: &str, a: &str) {
        let cpy = self.get(&a.to_string()).unwrap().clone();
        self.insert(s.to_string(), cpy);
    }
}

pub fn initialize_environment() -> Envt {
    let mut envt = ChainMap::new();
    operators::init(&mut envt);
    type_identification::init(&mut envt);
    envt
}
