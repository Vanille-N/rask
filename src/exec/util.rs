use crate::exec::Expr;
use std::rc::Rc;

pub enum EvalErr {
    UnknownIdentifier(String),
    UselessAntiquote(Rc<Expr>),
    CannotEval(Rc<Expr>),
    CannotApply(Rc<Expr>),
    ProperListRequired,
}
