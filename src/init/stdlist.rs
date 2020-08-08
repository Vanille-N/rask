use crate::exec::{eval, Envt, EvalErr, Expr};
use crate::init::*;
use std::rc::Rc;
use crate::list::List;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__cons"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() != 2 {
                return Err(EvalErr::WrongArgList);
            }
            let e = eval(args.head().unwrap().clone(), ctx)?;
            let l = eval(args.tail().head().unwrap().clone(), ctx)?;
            match &*l {
                Expr::List(lst) => Ok(Rc::new(Expr::List(Rc::new(lst.cons(e.clone()))))),
                Expr::Cons(lst, tl) => Ok(Rc::new(Expr::Cons(Rc::new(lst.cons(e.clone())), tl.clone()))),
                _ => Ok(Rc::new(Expr::Cons(Rc::new(List::from(vec![e])), l.clone()))),
            }
        })))
    );
    envt.insert(
        String::from("__car"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.head().is_none() || args.tail().head().is_some() {
                return Err(EvalErr::WrongArgList);
            }
            let l = eval(args.head().unwrap().clone(), ctx)?;
            match &*l {
                Expr::List(lst) => {
                    match lst.head() {
                        Some(h) => Ok(h.clone()),
                        None => Err(EvalErr::EmptyList),
                    }
                }
                Expr::Cons(lst, _) => {
                    match lst.head() {
                        Some(h) => Ok(h.clone()),
                        None => Err(EvalErr::EmptyList),
                    }
                }
                _ => Err(EvalErr::TypeError),
            }
        })))
    );
    envt.insert(
        String::from("__cdr"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.head().is_none() || args.tail().head().is_some() {
                return Err(EvalErr::WrongArgList);
            }
            let l = eval(args.head().unwrap().clone(), ctx)?;
            match &*l {
                Expr::List(lst) => {
                    match lst.head() {
                        Some(_) => Ok(Rc::new(Expr::List(Rc::new(lst.tail())))),
                        None => Err(EvalErr::EmptyList),
                    }
                }
                Expr::Cons(lst, e) => {
                    match lst.head() {
                        Some(_) => Ok(Rc::new(Expr::Cons(Rc::new(lst.tail()), e.clone()))),
                        None => Err(EvalErr::EmptyList),
                    }
                }
                _ => Err(EvalErr::TypeError),
            }
        })))
    );
    envt.insert(
        String::from("__list"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut v = Vec::new();
            for item in args.iter() {
                v.push(eval(item.clone(), ctx)?);
            }
            Ok(Rc::new(Expr::List(Rc::new(List::from(v)))))
        })))
    );
    envt.alias("cons", "__cons");
    envt.alias("car", "__car");
    envt.alias("cdr", "__cdr");
    envt.alias("list", "__list");
    define("
(define (cadr lst)
  (car (cdr lst)))
(define (caar lst)
  (car (car lst)))
(define (cdar lst)
  (cdr (car lst)))", envt);
}
