use crate::exec::{eval, Envt, EvalErr, Expr};
use std::rc::Rc;

pub fn apply(lst: &[Rc<Expr>], ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    if lst.is_empty() {
        return Ok(Rc::new(Expr::List(Rc::new(Vec::new()))));
    }
    match &*lst[0] {
        Expr::Char(_)
        | Expr::Integer(_)
        | Expr::Float(_)
        | Expr::String(_)
        | Expr::Cons(_, _)
        | Expr::Quote(_)
        | Expr::Quasiquote(_)
        | Expr::Antiquote(_) => Err(EvalErr::CannotApply(lst[0].clone())),
        Expr::List(_) => {
            match &*lst[0] {
                Expr::Atom(a) => apply_atom(&a, &lst[1..], ctx),
                Expr::List(_) => {
                    let result = eval(lst[0].clone(), ctx)?;
                    match &*result {
                        Expr::Func(f) => f(&lst[1..], ctx),
                        Expr::Atom(a) => apply_atom(&a, &lst[1..], ctx),
                        _ => Err(EvalErr::CannotApply(result.clone())),
                    }
                },
                _ => Err(EvalErr::CannotApply(lst[0].clone())),
            }
        }
        Expr::Atom(a) => apply_atom(&a, &lst[1..], ctx),
        _ => unreachable!(),
    }
}

#[allow(clippy::ptr_arg)]
pub fn apply_atom(
    a: &String,
    parameters: &[Rc<Expr>],
    ctx: &mut Envt,
) -> Result<Rc<Expr>, EvalErr> {
    match &a[..] {
        "define" => {
            println!("Yes");
            if parameters.is_empty() {
                return Err(EvalErr::EmptyDefine);
            }
            match &*parameters[0] {
                Expr::Atom(x) => {
                    if parameters.len() == 1 {
                        ctx.insert(x.to_string(), Rc::new(Expr::List(Rc::new(vec![])))); // Nil
                    } else if parameters.len() == 2 {
                        match eval(parameters[1].clone(), &mut ctx.extend()) {
                            Ok(res) => {
                                ctx.insert(x.to_string(), res.clone());
                                return Ok(Rc::new(Expr::List(Rc::new(vec![]))));
                            },
                            Err(err) => return Err(err),
                        }
                    } else {
                        return Err(EvalErr::InvalidDefine);
                    }
                },
    if let Some(f) = ctx.get(a) {
        match &*f {
            Expr::Func(f) => {
                let mut par = Vec::new();
                for p in parameters {
                    par.push(eval(p.clone(), ctx)?)
                }
                f(&par[..], ctx)
            }
            _ => Err(EvalErr::CannotApply(f.clone())),
        }
    } else {
        Err(EvalErr::UnknownIdent(Rc::new(Expr::Atom(Rc::new(
            a.clone(),
        )))))
    }
}
