use crate::exec::{Envt, EvalErr, Expr, eval};
use std::rc::Rc;
use crate::init::Alias;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__exp"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() != 1 {
                Err(EvalErr::WrongArgList)
            } else {
                match &*args[0] {
                    Expr::Integer(n) => Ok(Rc::new(Expr::Float((n as f64).exp()))),
                    Expr::Float(f) => Ok(Rc::new(Expr::Float(f.exp()))),
                    _ => Err(EvalErr::TypeError),
                }
            }
        })))
    );

    envt.alias("exp", "__exp");
    // envt.alias("pow", "__pow");
    // envt.alias("math.pi", "__pi");
    // envt.alias("math.e", "__e");
    // envt.alias("cos", "__cos");
    // envt.alias("sin", "__sin");
    // envt.alias("tan", "__tan");
}
