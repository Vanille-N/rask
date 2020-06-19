use crate::exec::Expr;
use std::rc::Rc;

#[derive(Debug)]
pub enum EvalErr {
    UnknownIdentifier(String),
    UselessAntiquote(Rc<Expr>),
    CannotEval(Rc<Expr>),
    CannotApply(Rc<Expr>),
    ProperListRequired(Rc<Expr>),
    UnknownIdent(Rc<Expr>),
    TypeError,
    WrongArgList,
}
