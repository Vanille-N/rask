use crate::exec::{EvalErr, Expr, eval, Envt};
use std::rc::Rc;

pub fn apply(lst: &Vec<Rc<Expr>>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    if lst.len() == 0 {
        return Ok(Rc::new(Expr::Nil));
    }
    match &*lst[0] {
        Expr::Nil | Expr::Char(_) | Expr::Integer(_) | Expr::Float(_) | Expr::String(_) | Expr::Cons(_, _) | Expr::Quote(_) | Expr::Quasiquote(_) | Expr::Antiquote(_) => Err(EvalErr::CannotApply(lst[0].clone())),
        _ => unreachable!(),
    }
}
    unimplemented!()
}
