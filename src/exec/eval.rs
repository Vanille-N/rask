use crate::exec::{apply, EvalErr, Expr, Envt};
use std::rc::Rc;

pub fn eval(expr: Rc<Expr>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    match &*expr {
        Expr::Quote(a) => Ok(a.clone()),
        Expr::Atom(a) => {
            match ctx.get(&a) {
                None => Err(EvalErr::UnknownIdentifier(a.to_string())),
                Some(x) => Ok(x.clone()),
            }
        }
        Expr::Antiquote(a) => Err(EvalErr::UselessAntiquote(a.clone())),
        Expr::Quasiquote(a) => quasi_eval(a.clone(), ctx),
        Expr::Integer(_) => Ok(expr.clone()),
        Expr::Float(_) => Ok(expr.clone()),
        Expr::Char(_) => Ok(expr.clone()),
        Expr::String(_) => Ok(expr.clone()),
        Expr::Nil => Ok(expr.clone()),
        Expr::Bool(_) => Ok(expr.clone()),
        Expr::Cons(_, _) => Err(EvalErr::ProperListRequired),
        Expr::List(items) => apply(&items, ctx),
        Expr::Func(_) |
        Expr::Ellipsis |
        Expr::Lambda(_, _) |
        Expr::Dot => Err(EvalErr::CannotEval(expr.clone())),
    }
}


pub fn quasi_eval(expr: Rc<Expr>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    match &*expr {
        Expr::Antiquote(a) => eval(a.clone(), ctx),
        Expr::Quasiquote(a) => quasi_eval(a.clone(), ctx),
        _ => Ok(expr.clone()),
    }
}
