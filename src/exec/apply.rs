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
        Expr::List(_) => match &*lst[0] {
            Expr::Atom(a) => apply_atom(&a, &lst[1..], ctx),
            Expr::List(_) => {
                let result = eval(lst[0].clone(), ctx)?;
                match &*result {
                    Expr::Func(f) => f(&lst[1..], ctx),
                    Expr::Atom(a) => apply_atom(&a, &lst[1..], ctx),
                    _ => Err(EvalErr::CannotApply(result.clone())),
                }
            }
            _ => Err(EvalErr::CannotApply(lst[0].clone())),
        },
        Expr::Atom(a) => apply_atom(&a, &lst[1..], ctx),
        _ => unreachable!(),
    }
}

pub fn apply_atom(
    a: &String,
    parameters: &[Rc<Expr>],
    ctx: &mut Envt,
) -> Result<Rc<Expr>, EvalErr> {
    if let Some(res) = apply_construct(a, parameters, ctx) {
        res
    } else if let Some(f) = ctx.get(a) {
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


fn is_bindable(name: &str) -> bool {
    match name {
        "define" | "let" | "letrec" => false,
        x if x.len() >= 2 && &x[..3] == "~~~" => false,
        _ => true,
    }
}
    match &a[..] {
        "define" => {
            if parameters.is_empty() {
                return Err(EvalErr::EmptyDefine);
            }
            match &*parameters[0] {
                Expr::Atom(x) if is_bindable(x) => {
                    if parameters.len() == 1 {
                        ctx.insert(x.to_string(), Rc::new(Expr::List(Rc::new(vec![]))));
                    // Nil
                    } else if parameters.len() == 2 {
                        match eval(parameters[1].clone(), &mut ctx.extend()) {
                            Ok(res) => {
                                ctx.insert(x.to_string(), res.clone());
                                return Ok(Rc::new(Expr::List(Rc::new(vec![]))));
                            }
                            Err(err) => return Err(err),
                        }
                    } else {
                        return Err(EvalErr::InvalidDefine);
                    }
                }
                Expr::Atom(x) => return Err(EvalErr::CannotBind(x.to_string())),
                Expr::List(fndef) => {
                    let mut ident = Vec::new();
                    for x in fndef.iter() {
                        match &**x {
                            Expr::Atom(name) if is_bindable(name) => ident.push(name.to_string()),
                            Expr::Atom(name) => return Err(EvalErr::CannotBind(name.to_string())),
                            _ => return Err(EvalErr::InvalidDefine),
                        }
                    }
                    if ident.is_empty() {
                        return Err(EvalErr::InvalidDefine);
                    }
                    let mut actions = Vec::new();
                    for act in &parameters[1..] {
                        actions.push(act.clone());
                    }
                    if actions.is_empty() {
                        return Err(EvalErr::InvalidDefine);
                    }
                    ctx.insert(
                        ident[0].to_string(),
                        Rc::new(Expr::Func(Rc::new(move |args, mut envt| {
                            if args.len() != ident.len() - 1 {
                                return Err(EvalErr::WrongArgList);
                            }
                            let mut ctx = envt.extend();
                            for i in 0..args.len() {
                                match eval(args[i].clone(), &mut envt) {
                                    Ok(val) => ctx.insert(ident[i + 1].clone(), val.clone()),
                                    Err(err) => return Err(err),
                                }
                            }
                            let mut res = Rc::new(Expr::List(Rc::new(vec![])));
                            for act in &actions {
                                match eval(act.clone(), &mut ctx) {
                                    Ok(val) => res = val,
                                    Err(err) => return Err(err),
                                }
                            }
                            Ok(res)
                        }))),
                    );
                    return Ok(Rc::new(Expr::List(Rc::new(vec![]))));
                }
                _ => return Err(EvalErr::InvalidDefine),
            }
        }
        "let" => {
            let mut new_envt = ctx.extend();
            if parameters.len() != 2 {
                return Err(EvalErr::WrongArgList);
            }
            if let Expr::List(bindings) = &*parameters[0] {
                for bind in bindings.iter() {
                    if let Expr::List(lst) = &**bind {
                        if lst.len() == 2 {
                            if let Expr::Atom(x) = &*lst[0] {
                                if is_bindable(&x) {
                                    match eval(lst[1].clone(), &mut ctx.extend()) {
                                        Ok(val) => {
                                            new_envt.insert(x.to_string(), val.clone());
                                        },
                                        Err(e) => return Err(e),
                                    }
                                } else {
                                    return Err(EvalErr::CannotBind(x.to_string()));
                                }
                            } else {
                                return Err(EvalErr::InvalidDefine);
                            }
                        } else {
                            return Err(EvalErr::WrongArgList);
                        }
                    } else {
                        return Err(EvalErr::InvalidDefine);
                    }
                }
            } else {
                return Err(EvalErr::TypeError);
            }
            return eval(parameters[1].clone(), &mut new_envt);
        },
        "let*" => {
            let mut new_envt = ctx.extend();
            if parameters.len() != 2 {
                return Err(EvalErr::WrongArgList);
            }
            if let Expr::List(bindings) = &*parameters[0] {
                for bind in bindings.iter() {
                    if let Expr::List(lst) = &**bind {
                        if lst.len() == 2 {
                            if let Expr::Atom(x) = &*lst[0] {
                                if is_bindable(&x) {
                                    match eval(lst[1].clone(), &mut new_envt) {
                                        Ok(val) => {
                                            new_envt.insert(x.to_string(), val.clone());
                                        },
                                        Err(e) => return Err(e),
                                    }
                                } else {
                                    return Err(EvalErr::CannotBind(x.to_string()));
                                }
                            } else {
                                return Err(EvalErr::InvalidDefine);
                            }
                        } else {
                            return Err(EvalErr::WrongArgList);
                        }
                    } else {
                        return Err(EvalErr::InvalidDefine);
                    }
                }
            } else {
                return Err(EvalErr::TypeError);
            }
            return eval(parameters[1].clone(), &mut new_envt);
        },
        _ => None,
    }
}
