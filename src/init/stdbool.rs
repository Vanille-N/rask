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
            match eval(args[0], ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Bool(b) => Ok(Rc::new(Expr::Bool(!b))),
                    _ => Err(EvalErr::TypeError),
                }
            }
        })))
    );
    envt.alias("not", "__not");
}
