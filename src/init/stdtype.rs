use crate::exec::{Envt, EvalErr, Expr, eval};
use std::rc::Rc;
use crate::init::Alias;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__integer?"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() > 1 {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args[0].clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Integer(_) => Ok(Rc::new(Expr::Bool(true))),
                    _ => Ok(Rc::new(Expr::Bool(false))),
                }
            }
        })))
    );
    envt.insert(
        String::from("__float?"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() > 1 {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args[0].clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Float(_) => Ok(Rc::new(Expr::Bool(true))),
                    _ => Ok(Rc::new(Expr::Bool(false))),
                }
            }
        })))
    );
    envt.insert(
        String::from("__real?"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() > 1 {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args[0].clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Integer(_) | Expr::Float(_) => Ok(Rc::new(Expr::Bool(true))),
                    _ => Ok(Rc::new(Expr::Bool(false))),
                }
            }
        })))
    );
    envt.insert(
        String::from("__bool?"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() > 1 {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args[0].clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Bool(_) => Ok(Rc::new(Expr::Bool(true))),
                    _ => Ok(Rc::new(Expr::Bool(false))),
                }
            }
        })))
    );
    envt.insert(
        String::from("__vector?"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() > 1 {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args[0].clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Vec(_) => Ok(Rc::new(Expr::Bool(true))),
                    _ => Ok(Rc::new(Expr::Bool(false))),
                }
            }
        })))
    );
    envt.insert(
        String::from("__character?"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() > 1 {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args[0].clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Char(_) => Ok(Rc::new(Expr::Bool(true))),
                    _ => Ok(Rc::new(Expr::Bool(false))),
                }
            }
        })))
    );
    envt.insert(
        String::from("__string?"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() > 1 {
                return Err(EvalErr::WrongArgList);
            }
            match eval(args[0].clone(), ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::String(_) => Ok(Rc::new(Expr::Bool(true))),
                    _ => Ok(Rc::new(Expr::Bool(false))),
                }
            }
        })))
    );
    envt.alias("integer?", "__integer?");
    envt.alias("float?", "__float?");
    envt.alias("real?", "__real?");
    envt.alias("bool?", "__bool?");
    envt.alias("vector?", "__vector?");
    envt.alias("character?", "__character?");
    envt.alias("string?", "__string?");
}
