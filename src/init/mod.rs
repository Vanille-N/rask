use crate::exec::Envt;
use chainmap::ChainMap;

mod operators;

pub fn initialize_environment() -> Envt {
    let mut envt = ChainMap::new();
    operators::init(&mut envt);
    envt
}
