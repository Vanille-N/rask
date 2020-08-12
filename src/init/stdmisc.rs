use crate::exec::{eval, Envt, EvalErr, Expr};
use crate::init::Alias;
use std::rc::Rc;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__quote"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.head().is_none() || args.tail().head().is_some() {
                return Err(EvalErr::WrongArgList);
            }
            Ok(args.head().unwrap().clone())
        }))),
    );
    envt.alias("quote", "__quote");
}
