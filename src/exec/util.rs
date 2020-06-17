use crate::exec::Expr;
use std::rc::Rc;

pub enum EvalErr {
    UnknownIdentifier(String),
    UselessAntiquote(Rc<Expr>),
    CannotEvalFn(Rc<Expr>),
    CannotEvalEllipsis,
    CannotEvalDot,
    ProperListRequired,
}
