use crate::exec::{apply, Envt, EvalErr, Expr};
use std::rc::Rc;
use crate::list::List;

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
        Expr::List(items) => apply((**items).clone(), ctx),
        Expr::Func(_) => Ok(expr.clone()),
        Expr::Ellipsis | Expr::Dot => Err(EvalErr::CannotEval(expr.clone())),
        Expr::Vec(_) => Ok(expr.clone()),
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
            Ok(Rc::new(Expr::List(Rc::new(List::from(v)))))
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
    use crate::init::initialize_environment;
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
        ( $( $elem:expr ),* ) => { Expr::List(Rc::new(List::from(vec![$( Rc::new($elem) ),*]))) }
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

    // Note that Rc::new(Expr::Dot) is merely a placeholder.
    // We check that an error of the correct type occured and assume that the
    // contents are correct as well.
    macro_rules! placeholder {
        () => {
            Rc::new(Expr::Dot)
        };
    }

    #[test]
    pub fn evals() {
        let mut envt = initialize_environment();
        envt.insert(String::from("a"), Rc::new(int!(12)));
        envt.insert(String::from("b"), Rc::new(float!(0.5)));
        envt.insert(String::from("c"), Rc::new(string!("xyz")));
        envt.insert(
            String::from("lst"),
            Rc::new(list!(atom!(a), atom!(b), atom!(c))),
        );
        check!("(define + __+)" [envt]-> "()");
        check!("(define - __-)" [envt]-> "()");
        check!("(define fn +)" [envt]-> "()");
        check!("(define * __*)" [envt]-> "()");

        envt.insert(
            String::from("fact"),
            Rc::new(Expr::Func(Rc::new(|args, envt| {
                if args.len() != 1 {
                    Err(EvalErr::WrongArgList)
                } else {
                    match eval(args.head().unwrap().clone(), envt) {
                        Ok(val) => {
                            println!("{:?}", val);
                            match *val {
                                Expr::Integer(i) => {
                                    let mut envt = envt.extend();
                                    envt.insert(String::from("i"), Rc::new(Expr::Integer(i)));
                                    match i {
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
                                }
                                _ => Err(EvalErr::TypeError),
                            }
                        }
                        Err(e) => Err(e),
                    }
                }
            }))),
        );
        envt.insert(
            String::from("set!"),
            Rc::new(Expr::Func(Rc::new(|args, envt| {
                if args.len() != 2 {
                    Err(EvalErr::WrongArgList)
                } else if let Expr::Atom(a) = &**args.head().unwrap() {
                    let newval = eval(args.tail().head().unwrap().clone(), envt).unwrap();
                    envt.update(&*a, newval);
                    Ok(Rc::new(Expr::List(Rc::new(List::new()))))
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
        check!("(fact 1)" [envt]-> "1");
        check!("(fact 3)" [envt]-> "6");
        check!("(set! a 4)" [envt]-> "()");
        check!("a" [envt]-> "4");
        check!("(+ 1 [fact (fact (+ -1 a))])" [envt]-> "721");
        check!("(set! fn fact)" [envt]-> "()");
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
        let mut envt = initialize_environment();
        check!("(define + __+)" [envt]-> "()");

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
        check!("(define e)" [envt]-> "()");
        check!("e" [envt]-> "()");

        err!("(h 55)" [envt]-> EvalErr::UnknownIdent(placeholder!()));
        err!("(g 55)" [envt]-> EvalErr::WrongArgList);
        err!("(define 4)" [envt]-> EvalErr::InvalidDefine);
        err!("(define (f x))" [envt]-> EvalErr::InvalidDefine);
        err!("(define (f 5))" [envt]-> EvalErr::InvalidDefine);
        err!("(define f g h)" [envt]-> EvalErr::InvalidDefine);
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
        let mut envt = initialize_environment();
        check!("(define + __+)" [envt]-> "()");
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
        check!("
(let [(x 1)
      (y 2)
      (z 3)]
     (let [(x (+ x 1))
           (y (+ x 2))]
          (+ (+ x y) z)))" [envt]-> "8");
        check!("
(let* [(x 1)
       (y (+ x 1))
       (z (+ y 1))
       (x (+ z 1))]
       `(,x ,y ,z))" [envt]-> "(4 2 3)");

        check!("(let [(undef 1)] ())" [envt]-> "()");
        err!("undef" [envt]-> EvalErr::UnknownIdent(placeholder!()));
        err!("(let [(undef 1) (y undef)] ())" [envt]-> EvalErr::UnknownIdent(placeholder!()));
        check!("
(define (f x)
    (define (g y)
        (+ y 1))
    (define (h y)
        (__* y 2))
    `(,(g x) ,(h x)))" [envt]-> "()");
        check!("(f 2)" [envt]-> "(3 4)");
        check!("
(let [(x 1) (y 2)]
    (define (add a b) (+ a b))
    (add x y))" [envt]-> "3");
        check!("
(let* [(x 1) (y (+ x 2))]
    (define (add a b) (+ a b))
    (add x y))" [envt]-> "4");
    }

    #[test]
    fn if_expr() {
        let mut envt = crate::init::initialize_environment();
        check!("(if #t 1 2)" [envt]-> "1");
        check!("(if #f 1 2)" [envt]-> "2");
        check!("(let [(x #t)] (if x 1 2))" [envt]-> "1");
        check!("(let [(x #f)] (if x 1 x))" [envt]-> "#f");
        check!("(if #t 1)" [envt]-> "1");
        check!("(if #f 1)" [envt]-> "()");

        err!("(if 1 () ())" [envt]-> EvalErr::TypeError);
        err!("(if #f () () ())" [envt]-> EvalErr::WrongArgList);
        err!("(if #f)" [envt]-> EvalErr::WrongArgList);

        check!("
(define (fact n)
    (if (__= n 0)
        1
        (__* n (fact (__- n 1)))))" [envt]-> "()");
        check!("`(,(fact 0) ,(fact 1) ,(fact 2) ,(fact 3))" [envt]-> "(1 1 2 6)");
    }

    #[test]
    fn operators() {
        let mut envt = crate::init::initialize_environment();
        check!("(+ 1 2)" [envt]-> "3");
        check!("(+ 1 2 3 4 5)" [envt]-> "15");
        check!("(+ 1.0 2 3 4 5)" [envt]-> "15.0");
        check!("(+)" [envt]-> "0");
        check!("(+ 1 -1)" [envt]-> "0");
        check!("(let [(+ __+)] (+ 1 2))" [envt]-> "3");
        check!("(* 2 5)" [envt]-> "10");
        check!("(* 1.5 2)" [envt]-> "3.0");
        check!("(* 3.0 3.0)" [envt]-> "9.0");
        check!("(* 0 0 undef)" [envt]-> "0");
        check!("(* 1 0 undef)" [envt]-> "0");
        check!("(- 0 1)" [envt]-> "-1");
        check!("(- 1 1 1 1)" [envt]-> "-2");
        check!("(- -3 -6)" [envt]-> "3");
        check!("(- -1.0 1.0)" [envt]-> "-2.0");
        check!("(/ 6 3)" [envt]-> "2");
        check!("(/ 6 3.0)" [envt]-> "2.0");
        check!("(/ 6 4)" [envt]-> "1.5");
        check!("(/ 6 3.0 2)" [envt]-> "1.0");
        err!("(+ \"abc\" 5)" [envt]-> EvalErr::TypeError);
        err!("(/ 1 0)" [envt]-> EvalErr::MathError);
        err!("(/ 1 0.0)" [envt]-> EvalErr::MathError);
    }

    #[test]
    fn comparisons() {
        let mut envt = crate::init::initialize_environment();
        check!("(< 1 2 3)" [envt]-> "#t");
        check!("(< 1 2 2)" [envt]-> "#f");
        check!("(<= 1 2 2)" [envt]-> "#t");
        check!("(= 2 2 2 2)" [envt]-> "#t");
        check!("(= 2 20e-1)" [envt]-> "#t");
        // Exhaustive tests
        check!("(< 2 2)" [envt]-> "#f");
        check!("(< 2 3)" [envt]-> "#t");
        check!("(< 3 2)" [envt]-> "#f");
        check!("(< 2.5 2.5)" [envt]-> "#f");
        check!("(< 2.5 3.5)" [envt]-> "#t");
        check!("(< 3.5 2.5)" [envt]-> "#f");
        check!("(< 2 2.0)" [envt]-> "#f");
        check!("(< 2.0 2)" [envt]-> "#f");
        check!("(< 2.0 3)" [envt]-> "#t");
        check!("(< 2 3.0)" [envt]-> "#t");
        check!("(< 3.0 2)" [envt]-> "#f");
        check!("(< 3 2.0)" [envt]-> "#f");

        check!("(<= 2 2)" [envt]-> "#t");
        check!("(<= 2 3)" [envt]-> "#t");
        check!("(<= 3 2)" [envt]-> "#f");
        check!("(<= 2.5 2.5)" [envt]-> "#t");
        check!("(<= 2.5 3.5)" [envt]-> "#t");
        check!("(<= 3.5 2.5)" [envt]-> "#f");
        check!("(<= 2 2.0)" [envt]-> "#t");
        check!("(<= 2.0 2)" [envt]-> "#t");
        check!("(<= 2.0 3)" [envt]-> "#t");
        check!("(<= 2 3.0)" [envt]-> "#t");
        check!("(<= 3.0 2)" [envt]-> "#f");
        check!("(<= 3 2.0)" [envt]-> "#f");

        check!("(> 2 2)" [envt]-> "#f");
        check!("(> 2 3)" [envt]-> "#f");
        check!("(> 3 2)" [envt]-> "#t");
        check!("(> 2.5 2.5)" [envt]-> "#f");
        check!("(> 2.5 3.5)" [envt]-> "#f");
        check!("(> 3.5 2.5)" [envt]-> "#t");
        check!("(> 2 2.0)" [envt]-> "#f");
        check!("(> 2.0 2)" [envt]-> "#f");
        check!("(> 2.0 3)" [envt]-> "#f");
        check!("(> 2 3.0)" [envt]-> "#f");
        check!("(> 3.0 2)" [envt]-> "#t");
        check!("(> 3 2.0)" [envt]-> "#t");

        check!("(>= 2 2)" [envt]-> "#t");
        check!("(>= 2 3)" [envt]-> "#f");
        check!("(>= 3 2)" [envt]-> "#t");
        check!("(>= 2.5 2.5)" [envt]-> "#t");
        check!("(>= 2.5 3.5)" [envt]-> "#f");
        check!("(>= 3.5 2.5)" [envt]-> "#t");
        check!("(>= 2 2.0)" [envt]-> "#t");
        check!("(>= 2.0 2)" [envt]-> "#t");
        check!("(>= 2.0 3)" [envt]-> "#f");
        check!("(>= 2 3.0)" [envt]-> "#f");
        check!("(>= 3.0 2)" [envt]-> "#t");
        check!("(>= 3 2.0)" [envt]-> "#t");
    }

    #[test]
    fn lambda() {
        let mut envt = crate::init::initialize_environment();
        check!("
((lambda x x) 1)" [envt]-> "1");
        check!("
((lambda (x y) x) 1 2)" [envt]-> "1");
        check!("
((lambda (x y z)
    (define add (lambda (a b) (+ a b)))
    (define w (add x y))
    (add w z))
 2 4 5)" [envt]-> "11");
    }

    #[test]
    fn exhaustive_bool_check() {
        let mut envt = crate::init::initialize_environment();
        check!("(and)" [envt]-> "#t");
        check!("(and #f)" [envt]-> "#f");
        check!("(and #t #t)" [envt]-> "#t");
        check!("(and (< 1 24) (= 2 2.0) #t)" [envt]-> "#t");
        check!("(or)" [envt]-> "#f");
        check!("(or #f)" [envt]-> "#f");
        check!("(or #f #f #f (= 1 1))" [envt]-> "#t");
        check!("(not #t)" [envt]-> "#f");
        check!("
(define xor
    (lambda (a b)
        (and (or a b) (not (and a b)))))" [envt]-> "()");
        check!("(xor #t #f)" [envt]-> "#t");
        check!("(xor #t #t)" [envt]-> "#f");
        check!("(xor #f #f)" [envt]-> "#f");
    }

    #[test]
    fn type_identification() {
        let mut envt = crate::init::initialize_environment();
        check!("(and (integer? 1) (not (float? 1)) (real? 1))" [envt]-> "#t");
        check!("(and (float? 1.) (not (integer? 1.)) (real? 1.))" [envt]-> "#t");
        check!("(vector? #())" [envt]-> "#t");
    }

    #[test]
    fn math_primitives() {
        let mut envt = crate::init::initialize_environment();
        check!("(exp 0)" [envt]-> "1.0");
        check!("(= (* (exp 1) (exp 2)) (exp (+ 1 2)))" [envt]-> "#t");
        check!("(pow 2 3)" [envt]-> "8");
        check!("(pow 3.0 2)" [envt]-> "9.0");
    }

    #[test]
    fn lists_operations() {
        let mut envt = crate::init::initialize_environment();
        check!("(define l '(1 2 3))" [envt]-> "()");
        check!("(cons 0 l)" [envt]-> "(0 1 2 3)");
        check!("(cons 0 '(1 2 3))" [envt]-> "(0 1 2 3)");
        check!("(cons '() '())" [envt]-> "(())");
        err!("(cons 1)" [envt]-> EvalErr::WrongArgList);
        err!("(cons 1 2 3)" [envt]-> EvalErr::WrongArgList);
        check!("(car l)" [envt]-> "1");
        check!("(car '(0 1))" [envt]-> "0");
        err!("(car '())" [envt]-> EvalErr::EmptyList);
        check!("(cdr l)" [envt]-> "(2 3)");
        check!("(cdr '(0 1))" [envt]-> "(1)");
        check!("(cdr '(0))" [envt]-> "()");
        err!("(cdr '())" [envt]-> EvalErr::EmptyList);
        check!("(cons 1 2)" [envt]-> "(1 . 2)");
        check!("(cons 0 (cons 1 (cons 2 3)))" [envt]-> "(0 1 2 . 3)");
        check!("(car '(1 . 2))" [envt]-> "1");
        check!("(cdr '(1 2 . 3))" [envt]-> "(2 . 3)");
        check!("(list 0 1 2 3)" [envt]-> "(0 1 2 3)");
        check!("(list #\\a '() '+)" [envt]-> "(#\\a () +)");
        check!("(list (car l) (car (cdr l)))" [envt]-> "(1 2)");
        check!("(list (car l) (cadr l))" [envt]-> "(1 2)");
        check!("(define lst '((0 1) (2 3)))" [envt]-> "()");
        check!("(car lst)" [envt]-> "(0 1)");
        check!("(cdr lst)" [envt]-> "((2 3))");
        check!("(caar lst)" [envt]-> "0");
        check!("(cadr lst)" [envt]-> "(2 3)");
        check!("(cdar lst)" [envt]-> "(1)");
        check!("(cddr lst)" [envt]-> "()");
        check!("(empty? '())" [envt]-> "#t");
        check!("(empty? '(1 . 2))" [envt]-> "#f");
        check!("(length '())" [envt]-> "0");
        check!("(length '(1 2 6 5))" [envt]-> "4");
        check!("(concat '() '(1 2))" [envt]-> "(1 2)");
        check!("(concat '(1 2 3) '(4 5 6))" [envt]-> "(1 2 3 4 5 6)");
    }

    #[test]
    fn set() {
        let mut envt = crate::init::initialize_environment();
        check!("(define a 1)" [envt]-> "()");
        check!("a" [envt]-> "1");
        check!("(set! a 2)" [envt]-> "()");
        check!("a" [envt]-> "2");
        check!("(define (f) (set! a 3))" [envt]-> "()");
        check!("a" [envt]-> "2");
        check!("(f)" [envt]-> "()");
        check!("a" [envt]-> "3");
        check!("(define (g x) (+ x a))" [envt]-> "()");
        check!("a" [envt]-> "3");
        check!("(g 1)" [envt]-> "4");
        check!("(set! a 4)" [envt]-> "()");
        check!("(g 1)" [envt]-> "5");
    }

    #[test]
    fn quote_fn() {
        let mut envt = crate::init::initialize_environment();
        check!("(quote a)" [envt]-> "a");
        check!("(quote 1)" [envt]-> "1");
        check!("(quote (+ 1 2))" [envt]-> "(+ 1 2)");
    }
}
