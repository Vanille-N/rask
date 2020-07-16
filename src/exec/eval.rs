use crate::exec::{apply, Envt, EvalErr, Expr};
use std::rc::Rc;

pub fn eval(expr: Rc<Expr>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    match &*expr {
        Expr::Quote(a) => Ok(a.clone()),
        Expr::Atom(a) => match ctx.get(&a) {
            None => Err(EvalErr::UnknownIdent(expr.clone())),
            Some(x) => Ok(x),
        },
        Expr::Antiquote(a) => Err(EvalErr::UselessAntiquote(a.clone())),
        Expr::Quasiquote(a) => quasi_eval(a.clone(), ctx),
        Expr::Integer(_) => Ok(expr.clone()),
        Expr::Float(_) => Ok(expr.clone()),
        Expr::Char(_) => Ok(expr.clone()),
        Expr::String(_) => Ok(expr.clone()),
        Expr::Bool(_) => Ok(expr.clone()),
        Expr::Cons(_, _) => Err(EvalErr::ProperListRequired(expr.clone())),
        Expr::List(items) => apply(&items, ctx),
        Expr::Func(_) => Ok(expr.clone()),
        Expr::Ellipsis | Expr::Dot => Err(EvalErr::CannotEval(expr.clone())),
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
#[cfg_attr(tarpaulin, skip)]
#[allow(unused_macros)]
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

    macro_rules! err {
        ( $s:tt [$envt:ident]-> $( $e:expr ),* ) => {
            let lt = parse(&$s);
            let target = vec![ $( $e ),* ];
            if target.len() != lt.len() {
                panic!("Not the right number of elements to compare: {} vs {} in \n {:?}", lt.len(), target.len(), &lt);
            }
            for i in 0..lt.len() {
                match &lt[i] {
                    Err(lt) => panic!("Expected {:?} but obtained {:?}", target, lt),
                    Ok(lt) => {
                        let result = eval(lt.clone(), &mut $envt);
                        if let Ok(e) = result {
                            panic!("Expected an error to occur: {:?}", e);
                        }
                        let result = result.err().unwrap();
                        if result != target[i] {
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
            Rc::new(Expr::Func(Rc::new(|args, _| {
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
        envt.insert(
            String::from("-"),
            Rc::new(Expr::Func(Rc::new(|args, _| {
                if args.len() != 2 {
                    Err(EvalErr::WrongArgList)
                } else if let Expr::Integer(i) = &*args[0] {
                    if let Expr::Integer(j) = &*args[1] {
                        Ok(Rc::new(Expr::Integer(i - j)))
                    } else {
                        Err(EvalErr::TypeError)
                    }
                } else {
                    Err(EvalErr::TypeError)
                }
            }))),
        );
        envt.insert(
            String::from("+"),
            Rc::new(Expr::Func(Rc::new(|args, _| {
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
        envt.insert(
            String::from("*"),
            Rc::new(Expr::Func(Rc::new(|args, _| {
                if args.len() != 2 {
                    Err(EvalErr::WrongArgList)
                } else if let Expr::Integer(i) = &*args[0] {
                    if let Expr::Integer(j) = &*args[1] {
                        Ok(Rc::new(Expr::Integer(i * j)))
                    } else {
                        Err(EvalErr::TypeError)
                    }
                } else {
                    Err(EvalErr::TypeError)
                }
            }))),
        );
        envt.insert(
            String::from("fact"),
            Rc::new(Expr::Func(Rc::new(|args, envt| {
                if args.len() != 1 {
                    Err(EvalErr::WrongArgList)
                } else if let Expr::Integer(i) = &*args[0] {
                    let mut envt = envt.extend();
                    envt.insert(String::from("i"), Rc::new(Expr::Integer(*i)));
                    match *i {
                        0 => Ok(Rc::new(Expr::Integer(1))),
                        _i => eval(
                            parse("(* i (fact (- i 1)))")[0]
                                .as_ref()
                                .ok()
                                .unwrap()
                                .clone(),
                            &mut envt,
                        ),
                    }
                } else {
                    Err(EvalErr::TypeError)
                }
            }))),
        );
        envt.insert(
            String::from("set!"),
            Rc::new(Expr::Func(Rc::new(|args, envt| {
                if args.len() != 2 {
                    Err(EvalErr::WrongArgList)
                } else if let Expr::Atom(a) = &*args[0] {
                    envt.update(&*a, args[1].clone());
                    Ok(Rc::new(Expr::List(Rc::new(vec![]))))
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
        check!("(+ 1 4)" [envt]-> "5");
        check!("(- 1 4)" [envt]-> "-3");
        check!("(fact 0)" [envt]-> "1");
        check!("(fact 3)" [envt]-> "6");
        check!("(set! 'a 4)" [envt]-> "()");
        check!("a" [envt]-> "4");
        check!("(+ 1 [fact (fact (+ -1 a))])" [envt]-> "721");
        check!("(set! 'fn fact)" [envt]-> "()");
        check!("(fn a)" [envt]-> "24");
        check!("()" [envt]-> "()");
        check!("1.1" [envt]-> "1.1");
        check!("#\\a" [envt]-> "#\\a");
        check!("\"abc\"" [envt]-> "\"abc\"");
        check!("#t" [envt]-> "#t");
        check!("`(`a)" [envt]-> "(a)");
        check!("'(a b c)" [envt]-> "(a b c)");
        check!("''a" [envt]-> "'a");
        check!("'`a" [envt]-> "`a");
        check!("',a" [envt]-> ",a");
        // Note that Rc::new(Expr::Dot) is merely a placeholder.
        // We check that an error of the correct type occured and assume that the
        // contents are correct as well.
        macro_rules! placeholder {
            () => {
                Rc::new(Expr::Dot)
            };
        }
        err!("('a b)" [envt]-> EvalErr::CannotApply(placeholder!()));
        err!("(#\\a a)" [envt]-> EvalErr::CannotApply(placeholder!()));
        err!("(() a)" [envt]-> EvalErr::CannotApply(placeholder!()));
        err!("(funct a)" [envt]-> EvalErr::UnknownIdent(placeholder!()));
        err!("(a a)" [envt]-> EvalErr::CannotApply(placeholder!()));
        err!(",a" [envt]-> EvalErr::UselessAntiquote(placeholder!()));
        err!("(a . b)" [envt]-> EvalErr::ProperListRequired(placeholder!()));
    }

    #[test]
    fn bindings() {
        // Define check
        let mut envt = ChainMap::new();
        envt.insert(
            String::from("+"),
            Rc::new(Expr::Func(Rc::new(|args, _| {
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
        check!("(define a 4)" [envt]-> "()");
        check!("(define s \"abc\")" [envt]-> "()");
        check!("s" [envt]-> "\"abc\"");
        check!("a" [envt]-> "4");
        check!("(define a 1)" [envt]-> "()");
        check!("a" [envt]-> "1");
        check!("(define (fun a b) (+ (+ a b) 1))" [envt]-> "()");
        check!("(fun (fun a 1) 2)" [envt]-> "6");
        check!("(define (sum a b c d e f g h) (+ (+ (+ a b) (+ c d)) (+ (+ e f) (+ g h))))" [envt]-> "()");
        check!("(sum 1 3 4 -1 2 -5 -5 1)" [envt]-> "0");
        check!("(define sum +)" [envt]-> "()");
        check!("(sum 1 2)" [envt]-> "3");
        check!("(define (f x) (g x x))" [envt]-> "()");
        check!("(define (g x y) (+ x y))" [envt]-> "()");
        check!("(f 3)" [envt]-> "6");
    }

    #[test]
    fn scoping() {
        let mut envt = ChainMap::new();
        check!("(define x 1)" [envt]-> "()");
        check!("(define y x)" [envt]-> "()");
        check!("y" [envt]-> "1");
        check!("(define x 2)" [envt]-> "()");
        check!("y" [envt]-> "1");

        check!("(define (f) x)" [envt]-> "()");
        check!("(define x 3)" [envt]-> "()");
        check!("(f)" [envt]-> "3");
    }

    #[test]
    fn let_bindings() {
        let mut envt = ChainMap::new();
        envt.insert(
            String::from("+"),
            Rc::new(Expr::Func(Rc::new(|args, _| {
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
        check!("(let [(x 2)] x)" [envt]-> "2");
        check!("(define x (let [(y 1) (z 2)] (+ y z)))" [envt]-> "()");
        check!("x" [envt]-> "3");
        check!("(let [(add +)] (add 1 2))" [envt]-> "3");
        check!("
(define (f x)
    (let [(plus1 (+ x 1))
          (plus2 (+ x 2))]
         (+ plus1 plus2)))" [envt]-> "()");
        check!("(f 2)" [envt]-> "7");

    }
}
