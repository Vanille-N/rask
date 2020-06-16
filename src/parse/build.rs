use crate::parse::{Expr, ParseErr, Token};

pub fn build(tokens: Vec<Token>) -> Vec<Result<Expr, ParseErr>> {
    let mut exprs = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        let expr = build_helper(&tokens, &mut idx);
        if expr.is_err() {
            exprs.push(expr);
            return exprs;
        }
        exprs.push(expr);
    }
    exprs
}

fn close_separator(op: &Token) -> Token {
    match op {
        Token::OpenParen => Token::CloseParen,
        Token::OpenBrace => Token::CloseBrace,
        _ => panic!("{:?} is not closable", op),
    }
}

fn build_helper(tokens: &[Token], idx: &mut usize) -> Result<Expr, ParseErr> {
    if *idx >= tokens.len() {
        return Err(ParseErr::Unfinished);
    }
    *idx += 1;
    match &tokens[*idx - 1] {
        op @ Token::OpenParen | op @ Token::OpenBrace => {
            let cl = close_separator(&op);
            let mut v = Vec::new();
            let mut dot_seen = false;
            while tokens[*idx] != cl {
                let expr = build_helper(tokens, idx)?;
                if let Expr::Dot = expr {
                    if dot_seen {
                        return Err(ParseErr::InvalidCons);
                    } else {
                        dot_seen = true;
                        // *idx += 1;
                        break;
                    }
                } else {
                    v.push(expr);
                }
                if *idx == tokens.len() {
                    return Err(match op {
                        Token::OpenParen => ParseErr::MismatchedOpenParen,
                        Token::OpenBrace => ParseErr::MismatchedOpenBrace,
                        _ => unreachable!(),
                    });
                }
            }
            if dot_seen {
                let expr = build_helper(tokens, idx)?;
                if *idx < tokens.len() && tokens[*idx] == cl {
                    *idx += 1;
                    Ok(Expr::Cons(v, Box::new(expr)))
                } else {
                    Err(ParseErr::InvalidCons)
                }
            } else {
                *idx += 1;
                Ok(Expr::List(v))
            }
        }
        Token::CloseParen => Err(ParseErr::MismatchedCloseParen),
        Token::CloseBrace => Err(ParseErr::MismatchedCloseBrace),
        Token::Quote => Ok(Expr::Quote(Box::new(build_helper(tokens, idx)?))),
        Token::Quasiquote => Ok(Expr::Quasiquote(Box::new(build_helper(tokens, idx)?))),
        Token::Antiquote => Ok(Expr::Antiquote(Box::new(build_helper(tokens, idx)?))),
        Token::Dot => Ok(Expr::Dot),
        Token::Ellipsis => Ok(Expr::Ellipsis),
        Token::Char(c) => Ok(Expr::Char(*c)),
        Token::Atom(a) => Ok(Expr::Atom(a.clone())),
        Token::Integer(i) => Ok(Expr::Integer(*i)),
        Token::Float(f) => Ok(Expr::Float(*f)),
        Token::Bool(b) => Ok(Expr::Bool(*b)),
        Token::String(s) => Ok(Expr::String(s.clone())),
        Token::Literal(l) => Ok(Expr::Literal(*l)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse::{corresponds, lex, split};

    macro_rules! check {
        ( $s:tt -> $( $e:expr ),* ) => {
            let sp = split($s);
            if let Err(e) = sp {
                panic!("Failed to split: {:?}", e);
            }
            let tokens = lex(sp.ok().unwrap());
            if let Err(e) = tokens {
                panic!("Failed to lex: {:?}", e);
            }
            let lt = build(tokens.ok().unwrap());
            let target: Vec<Result<Expr, ParseErr>> = vec![ $( $e ),* ];
            if target.len() != lt.len() {
                panic!("Not the right number of elements to compare: {} vs {} in \n {:?}", lt.len(), target.len(), &lt);
            }
            for i in 0..lt.len() {
                match &target[i] {
                    Err(target) => {
                        match &lt[i] {
                            Err(lt) => assert_eq!(target, lt),
                            Ok(lt) => panic!("Expected {:?} but obtained {:?}", target, lt),
                        }
                    }
                    Ok(target) => {
                        match &lt[i] {
                            Err(lt) => panic!("Expected {:?} but obtained {:?}", target, lt),
                            Ok(lt) => {
                                if !corresponds(&lt, &target) {
                                    panic!(
                                        "Parsing mistake:\n    {:?} is not the same as \n    {:?}",
                                        lt, target
                                    );
                                }
                            }
                        }
                    }
                }
            }
        };
    }

    macro_rules! err {
        ( $e:ident ) => { Err(ParseErr::$e) }
    }

    macro_rules! list {
        ( $( $elem:expr ),* ) => {
            Expr::List(vec![$( $elem ),*])
        }
    }

    macro_rules! atom {
        ( $( $elem:tt )* ) => {
            Expr::Atom(String::from(concat!($( stringify!($elem) ),*)))
        };
    }

    macro_rules! quote {
        ( $elem:expr ) => {
            Expr::Quote(Box::new($elem))
        };
    }

    macro_rules! quasiquote {
        ( $elem:expr ) => {
            Expr::Quasiquote(Box::new($elem))
        };
    }
    macro_rules! antiquote {
        ( $elem:expr ) => {
            Expr::Antiquote(Box::new($elem))
        };
    }

    macro_rules! int {
        ( $elem:expr ) => {
            Expr::Integer($elem)
        };
    }

    macro_rules! corresp {
        ( $lt:expr, $rt:expr ) => {
            assert!(corresponds(&$lt, &$rt))
        };
    }

    macro_rules! cons {
        ( $( $elem:expr ),* ; $end:expr ) => {Expr::Cons(vec![$( $elem ),*], Box::new($end))}
    }

    #[test]
    fn check_corresponds() {
        corresp!(atom!(a), atom!(a));
        corresp!(
            list!(atom!(a), atom!(b), atom!(c)),
            list!(atom!(a), atom!(b), atom!(c))
        );
    }

    #[test]
    fn simple_lists() {
        check!("(f x y)" -> Ok(list!(atom!(f), atom!(x), atom!(y))));
        check!("(f (g x) y z)" -> Ok(list!(atom!(f), list!(atom!(g), atom!(x)), atom!(y), atom!(z))));
        check!("(f '(g x) y z)" -> Ok(list!(atom!(f), quote!(list!(atom!(g), atom!(x))), atom!(y), atom!(z))));
        check!("(fff '(0 1 2 'x))" -> Ok(list!(atom!(fff), quote!(list!(int!(0), int!(1), int!(2), quote!(atom!(x)))))));
        check!("'()" -> Ok(quote!(list!())));
        check!("(let [(a 1) (b 2)] (+ (* a 2) (/ b -3)))" -> Ok(list!(atom!(let), list!(list!(atom!(a), int!(1)), list!(atom!(b), int!(2))), list!(atom!(+), list!(atom!(*), atom!(a), int!(2)), list!(atom!(/), atom!(b), int!(-3))))));
        check!("(a b . c)" -> Ok(cons!(atom!(a), atom!(b) ; atom!(c))));
    }

    #[test]
    fn build_failures() {
        check!("(a b c" -> err!(MismatchedOpenParen));
        check!("(a ]" -> err!(MismatchedCloseBrace));
        check!("(a . b . c)" -> err!(InvalidCons));
        check!("'" -> err!(Unfinished));
        check!("(       a)]" -> Ok(list!(atom!(a))), err!(MismatchedCloseBrace));
        check!("a b c)" -> Ok(atom!(a)), Ok(atom!(b)), Ok(atom!(c)), err!(MismatchedCloseParen));
    }
}
