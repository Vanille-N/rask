use crate::exec::{eval, is_bindable, Envt, EvalErr, Expr};
use std::rc::Rc;
use crate::list::List;

pub fn apply(lst: List<Rc<Expr>>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    if lst.is_empty() {
        return Ok(Rc::new(Expr::List(Rc::new(List::new()))));
    }
    match &**lst.head().unwrap() {
        Expr::Char(_)
        | Expr::Integer(_)
        | Expr::Float(_)
        | Expr::String(_)
        | Expr::Cons(_, _)
        | Expr::Quote(_)
        | Expr::Quasiquote(_)
        | Expr::Antiquote(_) => Err(EvalErr::CannotApply(lst.head().unwrap().clone())),
        Expr::List(_) => {
                let result = eval(lst.head().unwrap().clone(), ctx)?;
                match &*result {
                    Expr::Func(f) => f(lst.tail(), ctx),
                    Expr::Atom(a) => apply_atom(&a, lst.tail(), ctx),
                    _ => Err(EvalErr::CannotApply(result.clone())),
                }
        },
        Expr::Atom(a) => apply_atom(&a, lst.tail(), ctx),
        _ => unreachable!(),
    }
}

pub fn apply_atom(a: &str, parameters: List<Rc<Expr>>, ctx: &mut Envt) -> Result<Rc<Expr>, EvalErr> {
    if let Some(res) = apply_construct(a, parameters, ctx) {
        res
    } else if let Some(f) = ctx.get(&a.to_string()) {
        match &*f {
            Expr::Func(f) => f(parameters.clone(), ctx),
            _ => Err(EvalErr::CannotApply(f.clone())),
        }
    } else {
        Err(EvalErr::UnknownIdent(Rc::new(Expr::Atom(Rc::new(
            a.to_string(),
        )))))
    }
}

fn apply_construct(
    a: &str,
    parameters: List<Rc<Expr>>,
    ctx: &mut Envt,
) -> Option<Result<Rc<Expr>, EvalErr>> {
    match &a[..] {
        "define" => {
            if parameters.is_empty() {
                return Some(Err(EvalErr::EmptyDefine));
            }
            match &*parameters.head().unwrap().clone() {
                Expr::Atom(x) if is_bindable(x) => {
                    if parameters.tail().head().is_none() {
                        ctx.insert(x.to_string(), Rc::new(Expr::List(Rc::new(vec![]))));
                        Some(Ok(Rc::new(Expr::List(Rc::new(vec![])))))
                    } else if parameters.tail().tail().head().is_none() {
                        match eval(parameters[1].clone(), &mut ctx.extend()) {
                            Ok(res) => {
                                ctx.insert(x.to_string(), res);
                                Some(Ok(Rc::new(Expr::List(Rc::new(vec![])))))
                            }
                            Err(err) => Some(Err(err)),
                        }
                    } else {
                        Some(Err(EvalErr::InvalidDefine))
                    }
                }
                Expr::Atom(x) => Some(Err(EvalErr::CannotBind(x.to_string()))),
                Expr::List(fndef) => {
                    let mut ident = Vec::new();
                    for x in fndef.iter() {
                        match &**x {
                            Expr::Atom(name) if is_bindable(name) => ident.push(name.to_string()),
                            Expr::Atom(name) => {
                                return Some(Err(EvalErr::CannotBind(name.to_string())))
                            }
                            _ => return Some(Err(EvalErr::InvalidDefine)),
                        }
                    }
                    if ident.is_empty() {
                        return Some(Err(EvalErr::InvalidDefine));
                    }
                    let mut actions = Vec::new();
                    for act in &parameters[1..] {
                        actions.push(act.clone());
                    }
                    if actions.is_empty() {
                        return Some(Err(EvalErr::InvalidDefine));
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
                    Some(Ok(Rc::new(Expr::List(Rc::new(vec![])))))
                }
                _ => Some(Err(EvalErr::InvalidDefine)),
            }
        }
        "let" => {
            let mut new_envt = ctx.extend();
            if parameters.len() < 2 {
                return Some(Err(EvalErr::WrongArgList));
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
                                        }
                                        Err(e) => return Some(Err(e)),
                                    }
                                } else {
                                    return Some(Err(EvalErr::CannotBind(x.to_string())));
                                }
                            } else {
                                return Some(Err(EvalErr::InvalidDefine));
                            }
                        } else {
                            return Some(Err(EvalErr::WrongArgList));
                        }
                    } else {
                        return Some(Err(EvalErr::InvalidDefine));
                    }
                }
            } else {
                return Some(Err(EvalErr::TypeError));
            }
            let mut res = Rc::new(Expr::List(Rc::new(vec![])));
            for act in parameters[1..].iter() {
                match eval(act.clone(), &mut new_envt) {
                    Ok(val) => res = val,
                    Err(err) => return Some(Err(err)),
                }
            }
            Some(Ok(res))
        }
        "let*" => {
            let mut new_envt = ctx.extend();
            if parameters.len() < 2 {
                return Some(Err(EvalErr::WrongArgList));
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
                                        }
                                        Err(e) => return Some(Err(e)),
                                    }
                                } else {
                                    return Some(Err(EvalErr::CannotBind(x.to_string())));
                                }
                            } else {
                                return Some(Err(EvalErr::InvalidDefine));
                            }
                        } else {
                            return Some(Err(EvalErr::WrongArgList));
                        }
                    } else {
                        return Some(Err(EvalErr::InvalidDefine));
                    }
                }
            } else {
                return Some(Err(EvalErr::TypeError));
            }
            let mut res = Rc::new(Expr::List(Rc::new(vec![])));
            for act in parameters[1..].iter() {
                match eval(act.clone(), &mut new_envt) {
                    Ok(val) => res = val,
                    Err(err) => return Some(Err(err)),
                }
            }
            Some(Ok(res))
        }
        "if" => {
            if parameters.len() != 2 && parameters.len() != 3 {
                return Some(Err(EvalErr::WrongArgList));
            }
            match eval(parameters[0].clone(), &mut ctx.extend()) {
                Ok(val) => {
                    if let Expr::Bool(b) = &*val {
                        if *b {
                            Some(eval(parameters[1].clone(), &mut ctx.extend()))
                        } else if let Some(p) = parameters.get(2) {
                            Some(eval(p.clone(), &mut ctx.extend()))
                        } else {
                            Some(Ok(Rc::new(Expr::List(Rc::new(vec![])))))
                        }
                    } else {
                        Some(Err(EvalErr::TypeError))
                    }
                }
                Err(e) => Some(Err(e)),
            }
        }
        "lambda" => {
            if parameters.len() < 2 {
                return Some(Err(EvalErr::EmptyDefine));
            }
            let mut ident = Vec::new();
            match &*parameters[0] {
                Expr::Atom(name) => ident.push(name.to_string()),
                Expr::List(v) => {
                    for x in v.iter() {
                        match &**x {
                            Expr::Atom(name) if is_bindable(name) => ident.push(name.to_string()),
                            Expr::Atom(name) => {
                                return Some(Err(EvalErr::CannotBind(name.to_string())))
                            }
                            _ => return Some(Err(EvalErr::InvalidDefine)),
                        }
                    }
                }
                _ => return Some(Err(EvalErr::TypeError)),
            }
            if ident.is_empty() {
                return Some(Err(EvalErr::InvalidDefine));
            }
            let mut actions = Vec::new();
            for act in &parameters[1..] {
                actions.push(act.clone());
            }
            if actions.is_empty() {
                return Some(Err(EvalErr::InvalidDefine));
            }
            Some(Ok(Rc::new(Expr::Func(Rc::new(move |args, mut envt| {
                if args.len() != ident.len() {
                    return Err(EvalErr::WrongArgList);
                }
                let mut ctx = envt.extend();
                for i in 0..args.len() {
                    match eval(args[i].clone(), &mut envt) {
                        Ok(val) => ctx.insert(ident[i].clone(), val.clone()),
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
            })))))
        }
        _ => None,
    }
}
