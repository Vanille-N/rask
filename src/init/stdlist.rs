use crate::exec::{eval, Envt, EvalErr, Expr};
use crate::init::Alias;
use std::rc::Rc;

pub fn init(envt: &mut Envt) {
    envt.insert(
        String::from("__cons"),
        Rc::new(Expr::Func(Rc::new(|args, ctx| {
            if args.len() != 2 {
                return Err(EvalErr::WrongArgList);
            }
            let e = eval(args.head().unwrap().clone(), ctx)?;
            let l = eval(args.tail().head().unwrap().clone(), ctx)?;
            if let Expr::List(lst) = &*l {
                Ok(Rc::new(Expr::List(Rc::new(lst.cons(e.clone())))))
            } else {
                Err(EvalErr::TypeError)
            }
        })))
    );
    envt.alias("cons", "__cons");
}
