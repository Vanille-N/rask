use crate::exec::Expr;
use std::cmp;
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
    EmptyDefine,
    InvalidDefine,
    CannotBind(String),
    MathError,
    InvalidNumber,
    EmptyList,
}

// No need to test this, it will be correct in practice
#[cfg_attr(tarpaulin, skip)]
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
            EvalErr::EmptyDefine => identical!(EmptyDefine),
            EvalErr::InvalidDefine => identical!(InvalidDefine),
            EvalErr::CannotBind(_) => identical!(CannotBind(_)),
            EvalErr::MathError => identical!(MathError),
            EvalErr::InvalidNumber => identical!(InvalidNumber),
        }
    }
}

impl cmp::Eq for EvalErr {}

pub fn is_bindable(name: &str) -> bool {
    match name {
        "define" | "let" | "letrec" => false,
        x if x.len() >= 2 && &x[..2] == "__" => false,
        _ => true,
    }
}
