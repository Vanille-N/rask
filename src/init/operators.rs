use crate::exec::{Envt, EvalErr, Expr, eval};
use std::rc::Rc;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__+"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut sum = Expr::Integer(0);
            for x in args {
                match eval(x.clone(), ctx) {
                    Ok(val) => {
                        match *val {
                            Expr::Integer(n) => {
                                match sum {
                                    Expr::Integer(s) => sum = Expr::Integer(s + n),
                                    Expr::Float(f) => sum = Expr::Float(f + n as f64),
                                    _ => unreachable!(),
                                }
                            }
                            Expr::Float(y) => {
                                match sum {
                                    Expr::Integer(s) => sum = Expr::Float(s as f64 + y),
                                    Expr::Float(f) => sum = Expr::Float(f + y),
                                    _ => unreachable!(),
                                }
                            }
                            _ => return Err(EvalErr::TypeError),
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(sum))
        })))
    );
    envt.insert(
        String::from("__-"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut sum = match args.get(0) {
                None => return Ok(Rc::new(Expr::Integer(0))),
                Some(val) => match eval(val.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => Expr::Integer(n),
                        Expr::Float(f) => Expr::Float(f),
                        _ => return Err(EvalErr::TypeError),
                    }
                    Err(e) => return Err(e),
                }
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => {
                        match *val {
                            Expr::Integer(n) => {
                                match sum {
                                    Expr::Integer(s) => sum = Expr::Integer(s - n),
                                    Expr::Float(f) => sum = Expr::Float(f - n as f64),
                                    _ => unreachable!(),
                                }
                            }
                            Expr::Float(y) => {
                                match sum {
                                    Expr::Integer(s) => sum = Expr::Float(s as f64 - y),
                                    Expr::Float(f) => sum = Expr::Float(f - y),
                                    _ => unreachable!(),
                                }
                            }
                            _ => {
                                println!(">>> {:?}", val);
                                return Err(EvalErr::TypeError)
                            },
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(sum))
        })))
    );
    envt.insert(
        String::from("__*"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut sum = Expr::Integer(1);
            for x in args {
                match eval(x.clone(), ctx) {
                    Ok(val) => {
                        match *val {
                            Expr::Integer(0) => return Ok(Rc::new(Expr::Integer(0))),
                            Expr::Float(f) if f == 0.0 => return Ok(Rc::new(Expr::Float(0.0))),
                            Expr::Integer(n) => {
                                match sum {
                                    Expr::Integer(s) => sum = Expr::Integer(s * n),
                                    Expr::Float(f) => sum = Expr::Float(f * n as f64),
                                    _ => unreachable!(),
                                }
                            }
                            Expr::Float(y) => {
                                match sum {
                                    Expr::Integer(s) => sum = Expr::Float(s as f64 * y),
                                    Expr::Float(f) => sum = Expr::Float(f * y),
                                    _ => unreachable!(),
                                }
                            }
                            _ => return Err(EvalErr::TypeError),
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(sum))
        })))
    );
    envt.insert(
        String::from("__/"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut sum = match args.get(0) {
                None => return Ok(Rc::new(Expr::Integer(0))),
                Some(val) => match eval(val.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(0) => return Ok(Rc::new(Expr::Integer(0))),
                        Expr::Float(f) if f == 0.0 => return Ok(Rc::new(Expr::Float(0.0))),
                        Expr::Integer(n) => Expr::Integer(n),
                        Expr::Float(f) => Expr::Float(f),
                        _ => return Err(EvalErr::TypeError),
                    }
                    Err(e) => return Err(e),
                }
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => {
                        match *val {
                            Expr::Integer(0) => return Err(EvalErr::MathError),
                            Expr::Float(f) if f == 0.0 => return Err(EvalErr::MathError),
                            Expr::Integer(n) => {
                                match sum {
                                    Expr::Integer(0) => return Ok(Rc::new(sum)),
                                    Expr::Integer(s) => sum = if s % n == 0 { Expr::Integer(s / n) } else { Expr::Float(s as f64 / n as f64) },
                                    Expr::Float(f) => sum = Expr::Float(f / n as f64),
                                    _ => unreachable!(),
                                }
                            }
                            Expr::Float(y) => {
                                match sum {
                                    Expr::Integer(s) => sum = Expr::Float(s as f64 / y),
                                    Expr::Float(f) => sum = Expr::Float(f / y),
                                    _ => unreachable!(),
                                }
                            }
                            _ => return Err(EvalErr::TypeError),
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(sum))
        })))
    );
}
