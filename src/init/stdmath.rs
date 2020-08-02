use crate::exec::{Envt, EvalErr, Expr, eval};
use std::rc::Rc;
use crate::init::Alias;
use std::convert::TryInto;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__exp"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() != 1 {
                Err(EvalErr::WrongArgList)
            } else {
                match eval(args[0].clone(), ctx) {
                    Ok(val) => {
                        match &*val {
                            Expr::Integer(n) => Ok(Rc::new(Expr::Float((*n as f64).exp()))),
                            Expr::Float(f) => Ok(Rc::new(Expr::Float(f.exp()))),
                            _ => {
                                eprintln!("{:?}", args[0]);
                                Err(EvalErr::TypeError)
                            }
                        }
                    }
                    Err(e) => Err(e),
                }
            }
        })))
    );
    envt.insert(
        String::from("__pow"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() != 2 {
                Err(EvalErr::WrongArgList)
            } else {
                match eval(args[0].clone(), ctx) {
                    Ok(val) => {
                        match &*val {
                            Expr::Integer(n) => {
                                match eval(args[1].clone(), ctx) {
                                    Ok(val) => {
                                        match &*val {
                                            Expr::Integer(m) => {
                                                if *m >= 0 {
                                                    match (*m).try_into() {
                                                        Ok(u) => Ok(Rc::new(Expr::Integer(n.pow(u)))),
                                                        Err(e) => Err(EvalErr::InvalidNumber),
                                                    }
                                                } else {
                                                    match (*m).try_into() {
                                                        Ok(i) => Ok(Rc::new(Expr::Float((*n as f64).powi(i)))),
                                                        Err(e) => Err(EvalErr::InvalidNumber),
                                                    }
                                                }
                                            }
                                            Expr::Float(g) => Ok(Rc::new(Expr::Float((*n as f64).powf(*g)))),
                                            _ => Err(EvalErr::TypeError),
                                        }
                                    }
                                    Err(e) => Err(e),
                                }
                            }
                            Expr::Float(f) => match eval(args[1].clone(), ctx) {
                                Ok(val) => {
                                    match &*val {
                                        Expr::Integer(m) => {
                                            match (*m).try_into() {
                                                Ok(i) => Ok(Rc::new(Expr::Float(f.powi(i)))),
                                                Err(e) => Err(EvalErr::InvalidNumber),
                                            }
                                        }
                                        Expr::Float(g) => Ok(Rc::new(Expr::Float(f.powf(*g)))),
                                        _ => Err(EvalErr::TypeError),
                                    }
                                }
                                Err(e) => Err(e),
                            }
                            _ => {
                                eprintln!("{:?}", args[0]);
                                Err(EvalErr::TypeError)
                            }
                        }
                    }
                    Err(e) => Err(e),
                }
            }
        })))
    );
    envt.insert(
        String::from("__pi"),
        Rc::new(Expr::Float(3.14159265359))
    );
    envt.insert(
        String::from("__e"),
        Rc::new(Expr::Float(2.71828182845))
    );

    envt.alias("exp", "__exp");
    envt.alias("pow", "__pow");
    envt.alias("math.pi", "__pi");
    envt.alias("math.e", "__e");
    // envt.alias("cos", "__cos");
    // envt.alias("sin", "__sin");
    // envt.alias("tan", "__tan");
}
