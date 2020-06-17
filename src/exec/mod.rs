mod util;
mod eval;
mod apply;

pub use util::*;
pub use eval::eval;
pub use apply::apply;
pub use crate::parse::{Expr, Func, Envt};
