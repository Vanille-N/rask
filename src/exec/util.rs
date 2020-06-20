use crate::exec::Expr;
use std::rc::Rc;
use std::cmp;

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

impl cmp::PartialEq for EvalErr {
    fn eq(&self, other: &Self) -> bool {
        macro_rules! identical {
            ( $id:tt ) => {
                match other {
                    EvalErr::$id => true,
                    _ => false,
                }
            };
            ( $id:tt(_) ) => {
                match other {
                    EvalErr::$id(_) => true,
                    _ => false,
                }
            };
            ( $id:tt($contents:tt) ) => {
                match other {
                    EvalErr::$id(x) => x == $contents,
                    _ => false,
                }
            };
        }
        match self {
            EvalErr::UnknownIdent(_) => identical!(UnknownIdent(_)),
            EvalErr::UselessAntiquote(_) => identical!(UselessAntiquote(_)),
            EvalErr::CannotEval(_) => identical!(CannotEval(_)),
            EvalErr::CannotApply(_) => identical!(CannotApply(_)),
            EvalErr::ProperListRequired(_) => identical!(ProperListRequired(_)),
            EvalErr::TypeError => identical!(TypeError),
            EvalErr::WrongArgList => identical!(WrongArgList),
        }
    }
}

impl cmp::Eq for EvalErr {}
