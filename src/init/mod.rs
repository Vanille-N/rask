use crate::exec::Envt;
use chainmap::ChainMap;

mod stdbool;
mod stdmath;
mod stdnum;
mod stdtype;

pub trait Alias {
    fn alias(&mut self, s: &str, a: &str);
}

impl Alias for Envt {
    fn alias(&mut self, s: &str, a: &str) {
        let cpy = self.get(&a.to_string()).unwrap();
        self.insert(s.to_string(), cpy);
    }
}

pub fn initialize_environment() -> Envt {
    let mut envt = ChainMap::new();
    stdnum::init(&mut envt);
    stdtype::init(&mut envt);
    stdbool::init(&mut envt);
    stdmath::init(&mut envt);
    envt
}
