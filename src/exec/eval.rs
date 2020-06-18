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
        Expr::Cons(_, _) => Err(EvalErr::ProperListRequired(expr.clone())),
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::exec::EvalErr;
    use crate::parse::{parse, corresponds};
    use chainmap::ChainMap;

    macro_rules! err {
        ( $e:ident *) => {
            Err(ParseErr::$e(0))
        };
        ( $e:ident ) => {
            Err(ParseErr::$e)
        };
    }

    macro_rules! list {
        ( $( $elem:expr ),* ) => { Expr::List(Rc::new(vec![$( Rc::new($elem) ),*])) }
    }
    macro_rules! atom {
        ( $( $elem:tt )* ) => { Expr::Atom(Rc::new(String::from(concat!($( stringify!($elem) ),*)))) };
    }
    macro_rules! quote {
        ( $elem:expr ) => { Expr::Quote(Rc::new($elem)) };
    }
    macro_rules! quasiquote {
        ( $elem:expr ) => { Expr::Quasiquote(Rc::new($elem)) };
    }
    macro_rules! antiquote {
        ( $elem:expr ) => { Expr::Antiquote(Rc::new($elem)) };
    }
    macro_rules! int {
        ( $elem:expr ) => { Expr::Integer($elem) };
    }
    macro_rules! cons {
        ( $( $elem:expr ),* ; $end:expr ) => {Expr::Cons(Rc::new(vec![$( Rc::new($elem) ),*]), Rc::new($end))}
    }
    macro_rules! float {
        ( $elem:expr ) => { Expr::Float($elem) };
    }
    macro_rules! string {
        ( $e:expr ) => {
            Expr::String(Rc::new(String::from($e)))
        };
    }

    macro_rules! check {
        ( $s:tt [$envt:ident]-> $( $e:expr ),* ) => {
            let lt = parse(&$s);
            let target: Vec<Result<Expr, EvalErr>> = vec![ $( $e ),* ];
            if target.len() != lt.len() {
                panic!("Not the right number of elements to compare: {} vs {} in \n {:?}", lt.len(), target.len(), &lt);
            }
            for i in 0..lt.len() {
                match &target[i] {
                    Err(_) => unimplemented!(),
                    Ok(target) => {
                        match &lt[i] {
                            Err(lt) => panic!("Expected {:?} but obtained {:?}", target, lt),
                            Ok(lt) => {
                                let result = eval(lt.clone(), &mut $envt).ok().unwrap();
                                if !corresponds(&result, &target) {
                                    panic!(
                                        "Parsing mistake:\n    {:?} is not the same as \n    {:?}",
                                        lt, target
                                    );
                                }
                            }
                        }
                    }
                }
            }
        };
    }
