use crate::exec::{Envt, eval};
use crate::parse::parse;
use chainmap::ChainMap;

mod stdbool;
mod stdmath;
mod stdnum;
mod stdtype;
mod stdlist;

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
    stdlist::init(&mut envt);
    envt
}


pub fn define(src: &str, envt: &mut Envt) {
    let lt = parse(src);
    for i in 0..lt.len() {
        match &lt[i] {
            Err(_) => panic!("Failed to define"),
            Ok(lt) => {
                eval(lt.clone(), envt).expect("Failed to evaluate");
            }
        }
    }
}
