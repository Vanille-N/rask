use crate::exec::{eval, Envt, EvalErr, Expr};
use crate::init::Alias;
use std::rc::Rc;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__+"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut sum = Expr::Integer(0);
            for x in args {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => match sum {
                            Expr::Integer(s) => sum = Expr::Integer(s + n),
                            Expr::Float(f) => sum = Expr::Float(f + n as f64),
                            _ => unreachable!(),
                        },
                        Expr::Float(y) => match sum {
                            Expr::Integer(s) => sum = Expr::Float(s as f64 + y),
                            Expr::Float(f) => sum = Expr::Float(f + y),
                            _ => unreachable!(),
                        },
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(sum))
        }))),
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
                    },
                    Err(e) => return Err(e),
                },
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => match sum {
                            Expr::Integer(s) => sum = Expr::Integer(s - n),
                            Expr::Float(f) => sum = Expr::Float(f - n as f64),
                            _ => unreachable!(),
                        },
                        Expr::Float(y) => match sum {
                            Expr::Integer(s) => sum = Expr::Float(s as f64 - y),
                            Expr::Float(f) => sum = Expr::Float(f - y),
                            _ => unreachable!(),
                        },
                        _ => {
                            println!(">>> {:?}", val);
                            return Err(EvalErr::TypeError);
                        }
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(sum))
        }))),
    );
    envt.insert(
        String::from("__*"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut sum = Expr::Integer(1);
            for x in args {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(0) => return Ok(Rc::new(Expr::Integer(0))),
                        Expr::Float(f) if f == 0.0 => return Ok(Rc::new(Expr::Float(0.0))),
                        Expr::Integer(n) => match sum {
                            Expr::Integer(s) => sum = Expr::Integer(s * n),
                            Expr::Float(f) => sum = Expr::Float(f * n as f64),
                            _ => unreachable!(),
                        },
                        Expr::Float(y) => match sum {
                            Expr::Integer(s) => sum = Expr::Float(s as f64 * y),
                            Expr::Float(f) => sum = Expr::Float(f * y),
                            _ => unreachable!(),
                        },
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(sum))
        }))),
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
                    },
                    Err(e) => return Err(e),
                },
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(0) => return Err(EvalErr::MathError),
                        Expr::Float(f) if f == 0.0 => return Err(EvalErr::MathError),
                        Expr::Integer(n) => match sum {
                            Expr::Integer(0) => return Ok(Rc::new(sum)),
                            Expr::Integer(s) => {
                                sum = if s % n == 0 {
                                    Expr::Integer(s / n)
                                } else {
                                    Expr::Float(s as f64 / n as f64)
                                }
                            }
                            Expr::Float(f) => sum = Expr::Float(f / n as f64),
                            _ => unreachable!(),
                        },
                        Expr::Float(y) => match sum {
                            Expr::Integer(s) => sum = Expr::Float(s as f64 / y),
                            Expr::Float(f) => sum = Expr::Float(f / y),
                            _ => unreachable!(),
                        },
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(sum))
        }))),
    );
    envt.insert(
        String::from("__="),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let fst = match args.get(0) {
                None => return Err(EvalErr::WrongArgList),
                Some(val) => match eval(val.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => Expr::Integer(n),
                        Expr::Float(f) => Expr::Float(f),
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                },
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => match fst {
                            Expr::Integer(s) => {
                                if s != n {
                                    return Ok(Rc::new(Expr::Bool(false)));
                                }
                            }
                            #[allow(clippy::float_cmp)]
                            Expr::Float(f) => {
                                if f != n as f64 {
                                    return Ok(Rc::new(Expr::Bool(false)));
                                }
                            }
                            _ => unreachable!(),
                        },
                        Expr::Float(y) => match fst {
                            #[allow(clippy::float_cmp)]
                            Expr::Integer(s) => {
                                if s as f64 != y {
                                    return Ok(Rc::new(Expr::Bool(false)));
                                }
                            }
                            #[allow(clippy::float_cmp)]
                            Expr::Float(f) => {
                                if f != y {
                                    return Ok(Rc::new(Expr::Bool(false)));
                                }
                            }
                            _ => unreachable!(),
                        },
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(Expr::Bool(true)))
        }))),
    );
    envt.insert(
        String::from("__<"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut pred = match args.get(0) {
                None => return Err(EvalErr::WrongArgList),
                Some(val) => match eval(val.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => Expr::Integer(n),
                        Expr::Float(f) => Expr::Float(f),
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                },
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => {
                            match pred {
                                Expr::Integer(s) => {
                                    if s >= n {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                Expr::Float(f) => {
                                    if f >= n as f64 {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                _ => unreachable!(),
                            }
                            pred = Expr::Integer(n);
                        }
                        Expr::Float(y) => {
                            match pred {
                                Expr::Integer(s) => {
                                    if (s as f64) >= y {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                Expr::Float(f) => {
                                    if f >= y {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                _ => unreachable!(),
                            }
                            pred = Expr::Float(y);
                        }
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(Expr::Bool(true)))
        }))),
    );
    envt.insert(
        String::from("__<="),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut pred = match args.get(0) {
                None => return Err(EvalErr::WrongArgList),
                Some(val) => match eval(val.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => Expr::Integer(n),
                        Expr::Float(f) => Expr::Float(f),
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                },
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => {
                            match pred {
                                Expr::Integer(s) => {
                                    if s > n {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                Expr::Float(f) => {
                                    if f > n as f64 {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                _ => unreachable!(),
                            }
                            pred = Expr::Integer(n);
                        }
                        Expr::Float(y) => {
                            match pred {
                                Expr::Integer(s) => {
                                    if (s as f64) > y {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                Expr::Float(f) => {
                                    if f > y {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                _ => unreachable!(),
                            }
                            pred = Expr::Float(y);
                        }
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(Expr::Bool(true)))
        }))),
    );
    envt.insert(
        String::from("__>"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut pred = match args.get(0) {
                None => return Err(EvalErr::WrongArgList),
                Some(val) => match eval(val.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => Expr::Integer(n),
                        Expr::Float(f) => Expr::Float(f),
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                },
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => {
                            match pred {
                                Expr::Integer(s) => {
                                    if s <= n {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                Expr::Float(f) => {
                                    if f <= n as f64 {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                _ => unreachable!(),
                            }
                            pred = Expr::Integer(n);
                        }
                        Expr::Float(y) => {
                            match pred {
                                Expr::Integer(s) => {
                                    if (s as f64) <= y {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                Expr::Float(f) => {
                                    if f <= y {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                _ => unreachable!(),
                            }
                            pred = Expr::Float(y);
                        }
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(Expr::Bool(true)))
        }))),
    );
    envt.insert(
        String::from("__>="),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            let mut pred = match args.get(0) {
                None => return Err(EvalErr::WrongArgList),
                Some(val) => match eval(val.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => Expr::Integer(n),
                        Expr::Float(f) => Expr::Float(f),
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                },
            };
            for x in args.iter().skip(1) {
                match eval(x.clone(), ctx) {
                    Ok(val) => match *val {
                        Expr::Integer(n) => {
                            match pred {
                                Expr::Integer(s) => {
                                    if s < n {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                Expr::Float(f) => {
                                    if f < n as f64 {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                _ => unreachable!(),
                            }
                            pred = Expr::Integer(n);
                        }
                        Expr::Float(y) => {
                            match pred {
                                Expr::Integer(s) => {
                                    if (s as f64) < y {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                Expr::Float(f) => {
                                    if f < y {
                                        return Ok(Rc::new(Expr::Bool(false)));
                                    }
                                }
                                _ => unreachable!(),
                            }
                            pred = Expr::Float(y);
                        }
                        _ => return Err(EvalErr::TypeError),
                    },
                    Err(e) => return Err(e),
                }
            }
            Ok(Rc::new(Expr::Bool(true)))
        }))),
    );
    envt.alias("+", "__+");
    envt.alias("-", "__-");
    envt.alias("*", "__*");
    envt.alias("/", "__/");
    envt.alias("=", "__=");
    envt.alias("<", "__<");
    envt.alias(">", "__>");
    envt.alias("<=", "__<=");
    envt.alias(">=", "__>=");
}
