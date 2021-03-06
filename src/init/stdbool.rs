use crate::exec::{eval, Envt, EvalErr, Expr};
use crate::init::Alias;
use std::rc::Rc;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__not"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.head().is_none() || args.tail().head().is_some() {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args.head().unwrap().clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Bool(b) => Ok(Rc::new(Expr::Bool(!b))),
                    _ => Err(EvalErr::TypeError),
                },
            }
        }))),
    );
    envt.insert(
        String::from("__and"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            for x in args.iter() {
                match eval(x.clone(), ctx) {
                    Err(e) => return Err(e),
                    Ok(val) => match &*val {
                        Expr::Bool(false) => return Ok(Rc::new(Expr::Bool(false))),
                        Expr::Bool(_) => (),
                        _ => return Err(EvalErr::TypeError),
                    },
                }
            }
            Ok(Rc::new(Expr::Bool(true)))
        }))),
    );
    envt.insert(
        String::from("__or"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            for x in args.iter() {
                match eval(x.clone(), ctx) {
                    Err(e) => return Err(e),
                    Ok(val) => match &*val {
                        Expr::Bool(true) => return Ok(Rc::new(Expr::Bool(true))),
                        Expr::Bool(_) => (),
                        _ => return Err(EvalErr::TypeError),
                    },
                }
            }
            Ok(Rc::new(Expr::Bool(false)))
        }))),
    );
    envt.alias("not", "__not");
    envt.alias("and", "__and");
    envt.alias("or", "__or");
}
