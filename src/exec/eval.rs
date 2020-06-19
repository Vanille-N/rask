use crate::exec::{apply, Envt, EvalErr, Expr};
use std::rc::Rc;

pub fn eval(expr: Rc<Expr>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    match &*expr {
        Expr::Quote(a) => Ok(a.clone()),
        Expr::Atom(a) => match ctx.get(&a) {
            None => Err(EvalErr::UnknownIdentifier(a.to_string())),
            Some(x) => Ok(x.clone()),
        },
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
        Expr::Func(_) | Expr::Ellipsis | Expr::Dot => Err(EvalErr::CannotEval(expr.clone())),
    }
}

pub fn quasi_eval(expr: Rc<Expr>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    match &*expr {
        Expr::Antiquote(a) => eval(a.clone(), ctx),
        Expr::Quasiquote(a) => quasi_eval(a.clone(), ctx),
        Expr::List(items) => {
            let mut v = Vec::new();
            for i in items.iter() {
                v.push(quasi_eval(i.clone(), ctx)?);
            }
            Ok(Rc::new(Expr::List(Rc::new(v))))
        }
        _ => Ok(expr.clone()),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::exec::EvalErr;
    use crate::parse::{corresponds, parse};
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
        ( $elem:expr ) => {
            Expr::Quote(Rc::new($elem))
        };
    }
    macro_rules! quasiquote {
        ( $elem:expr ) => {
            Expr::Quasiquote(Rc::new($elem))
        };
    }
    macro_rules! antiquote {
        ( $elem:expr ) => {
            Expr::Antiquote(Rc::new($elem))
        };
    }
    macro_rules! int {
        ( $elem:expr ) => {
            Expr::Integer($elem)
        };
    }
    macro_rules! cons {
        ( $( $elem:expr ),* ; $end:expr ) => {Expr::Cons(Rc::new(vec![$( Rc::new($elem) ),*]), Rc::new($end))}
    }
    macro_rules! float {
        ( $elem:expr ) => {
            Expr::Float($elem)
        };
    }
    macro_rules! chr {
        ( $elem:expr ) => {
            Expr::Char($elem)
        };
    }
    macro_rules! string {
        ( $e:expr ) => {
            Expr::String(Rc::new(String::from($e)))
        };
    }

    macro_rules! check {
        ( $s:tt [$envt:ident]-> $( $e:expr ),* ) => {
            let lt = parse(&$s);
            let target: Vec<Rc<Expr>> = vec![ $( parse(&$e)[0].as_ref().ok().unwrap().clone() ),* ];
            if target.len() != lt.len() {
                panic!("Not the right number of elements to compare: {} vs {} in \n {:?}", lt.len(), target.len(), &lt);
            }
            for i in 0..lt.len() {
                match &lt[i] {
                    Err(lt) => panic!("Expected {:?} but obtained {:?}", target, lt),
                    Ok(lt) => {
                        let result = eval(lt.clone(), &mut $envt);
                        if let Err(e) = result {
                            panic!("Failed to evaluate: {:?}", e);
                        }
                        let result = result.ok().unwrap();
                        if !corresponds(&result, &target[i]) {
                            panic!(
                                "Parsing mistake:\n    {:?} is not the same as \n    {:?}",
                                &result, target
                            );
                        }
                    }
                }
            }
        };
    }

    #[test]
    pub fn evals() {
        let mut envt = ChainMap::new();
        envt.insert(String::from("a"), Rc::new(int!(12)));
        envt.insert(String::from("b"), Rc::new(float!(0.5)));
        envt.insert(String::from("c"), Rc::new(string!("xyz")));
        envt.insert(
            String::from("lst"),
            Rc::new(list!(atom!(a), atom!(b), atom!(c))),
        );
        envt.insert(
            String::from("fn"),
            Rc::new(Expr::Func(Rc::new(|args| {
                if args.len() != 2 {
                    Err(EvalErr::WrongArgList)
                } else if let Expr::Integer(i) = &*args[0] {
                    if let Expr::Integer(j) = &*args[1] {
                        Ok(Rc::new(Expr::Integer(i + j)))
                    } else {
                        Err(EvalErr::TypeError)
                    }
                } else {
                    Err(EvalErr::TypeError)
                }
            }))),
        );
        check!("a" [envt]-> "12");
        check!("'b" [envt]-> "b");
        check!("`a" [envt]-> "a");
        check!("`lst" [envt]-> "lst");
        check!("`,b" [envt]-> "0.5");
        check!("'lst" [envt]-> "lst");
        check!("'(a b)" [envt]-> "(a b)");
        check!("'(1 2 \"c\" #\\')" [envt]-> "(1 2 \"c\" #\\')");
        check!("lst" [envt]-> "(a b c)");
        check!("`,lst" [envt]-> "(a b c)");
        check!("`(,a b)" [envt]-> "(12 b)");
        check!("`(a ,b ,lst)" [envt]-> "(a 0.5 (a b c))");
        check!("(fn 1 2)" [envt]-> "3");
        check!("(fn a -1)" [envt]-> "11");
    }
}
