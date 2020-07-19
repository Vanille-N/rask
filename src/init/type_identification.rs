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
            match eval(args[0], ctx) {
                Err(e) => Err(e),
                Ok(val) => match &*val {
                    Expr::Integer(_) => Ok(Rc::new(Expr::Bool(true))),
                    _ => Ok(Rc::new(Expr::Bool(false))),
                }
            }
        })))
    );
    envt.alias("integer?", "__integer?");
}
