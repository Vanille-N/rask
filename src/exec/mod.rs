mod apply;
mod eval;
mod util;

pub use crate::parse::{Envt, Expr, Func};
pub use apply::apply;
pub use eval::eval;
pub use util::*;
