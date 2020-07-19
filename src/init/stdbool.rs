use crate::exec::{Envt, EvalErr, Expr, eval};
use std::rc::Rc;
use crate::init::Alias;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__not"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() > 1 {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args[0].clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Bool(b) => Ok(Rc::new(Expr::Bool(!b))),
                    _ => Err(EvalErr::TypeError),
                }
            }
        })))
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
                    }
                }
            }
            Ok(Rc::new(Expr::Bool(true)))
        })))
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
                    }
                }
            }
            Ok(Rc::new(Expr::Bool(false)))
        })))
    );
    envt.alias("not", "__not");
    envt.alias("and", "__and");
    envt.alias("or", "__or");
}
