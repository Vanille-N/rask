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
        Expr::Ellipsis => Err(EvalErr::CannotEvalEllipsis),
        Expr::Nil => Ok(expr.clone()),
        Expr::Func(_) => Err(EvalErr::CannotEvalFn(expr.clone())),
        Expr::Lambda(_, _) => Err(EvalErr::CannotEvalFn(expr.clone())),
        Expr::Dot => Err(EvalErr::CannotEvalDot),
        Expr::Bool(_) => Ok(expr.clone()),
        Expr::Cons(_, _) => Err(EvalErr::ProperListRequired),
        Expr::List(items) => apply(&items),
    }
}


pub fn quasi_eval(expr: Rc<Expr>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    match &*expr {
        Expr::Antiquote(a) => eval(a.clone(), ctx),
        Expr::Quasiquote(a) => quasi_eval(a.clone(), ctx),
        _ => Ok(expr.clone()),
    }
}
