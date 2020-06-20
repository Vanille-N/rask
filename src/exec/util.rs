use crate::exec::Expr;
use std::rc::Rc;

#[derive(Debug)]
pub enum EvalErr {
    UnknownIdent(Rc<Expr>),
    UselessAntiquote(Rc<Expr>),
    CannotEval(Rc<Expr>),
    CannotApply(Rc<Expr>),
    ProperListRequired(Rc<Expr>),
    TypeError,
    WrongArgList,
}
