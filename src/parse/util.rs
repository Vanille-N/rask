use chainmap::ChainMap;
use std::rc::Rc;
use std::{cmp, fmt};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErr {
    UnterminatedString(usize),
    IncorrectSpacing(usize),
    LoneNumbersign,
    InvalidChar(String),
    InvalidLiteral(String),
    InvalidIdent(String),
    UnterminatedComment,
    NoCommentStart,
    MismatchedOpenParen,
    MismatchedCloseParen,
    MismatchedOpenBrace,
    MismatchedCloseBrace,
    Unfinished,
    InvalidCons,
}

pub enum EvalErr {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Literal {
    LoadSource,
    Exit,
    Show,
}

#[derive(Debug)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Quote,
    Quasiquote,
    Antiquote,
    Dot,
    Ellipsis,
    Char(char),
    Atom(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Literal(Literal),
}

impl cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        macro_rules! identical {
            ( $id:tt ) => {
                match other {
                    Token::$id => true,
                    _ => false,
                }
            };
            ( $id:tt($contents:tt) ) => {
                match other {
                    Token::$id(x) => x == $contents,
                    _ => false,
                }
            };
        }
        match self {
            Token::OpenBrace => identical!(OpenBrace),
            Token::CloseBrace => identical!(CloseBrace),
            Token::OpenParen => identical!(OpenParen),
            Token::CloseParen => identical!(CloseParen),
            Token::Quote => identical!(Quote),
            Token::Quasiquote => identical!(Quasiquote),
            Token::Antiquote => identical!(Antiquote),
            Token::Dot => identical!(Dot),
            Token::Char(c) => identical!(Char(c)),
            Token::Atom(s) => identical!(Atom(s)),
            Token::Integer(i) => identical!(Integer(i)),
            Token::Bool(b) => identical!(Bool(b)),
            Token::String(s) => identical!(String(s)),
            Token::Literal(lit) => identical!(Literal(lit)),
            Token::Ellipsis => identical!(Ellipsis),
            Token::Float(f) => match other {
                Token::Float(g) => (g - f).abs() < 0.00000001,
                _ => false,
            },
        }
    }
}

pub enum Expr {
    Atom(String),
    List(Vec<Expr>),
    Quote(Box<Expr>),
    Quasiquote(Box<Expr>),
    Antiquote(Box<Expr>),
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Func(Func),
    Lambda(Func, Envt),
    Nil,
    Ellipsis,
    Dot,
    Bool(bool),
    Literal(Literal),
    Cons(Vec<Expr>, Box<Expr>),
}

pub struct Func(Box<dyn Fn(Vec<Expr>) -> Result<Expr, EvalErr>>);

pub struct Envt(ChainMap<String, Rc<Expr>>);

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expr::Atom(s) => write!(f, "Atom({:?})", &s),
            Expr::List(v) => {
                if v.len() == 0 {
                    write!(f, "'()")
                } else {
                    write!(f, "({:?}", v[0])?;
                    for item in v.iter().skip(1) {
                        write!(f, " {:?}", &item)?;
                    }
                    write!(f, ")")
                }
            }
            Expr::Quote(e) => write!(f, "'{:?}", &e),
            Expr::Quasiquote(e) => write!(f, "`{:?}", &e),
            Expr::Antiquote(e) => write!(f, ",{:?}", &e),
            Expr::Integer(i) => write!(f, "Integer({})", i),
            Expr::Float(x) => write!(f, "Float({})", x),
            Expr::String(s) => write!(f, "\"{}\"", &s),
            Expr::Char(c) => write!(f, "#\\{:?}", c),
            Expr::Func(_) => write!(f, "<fun>"),
            Expr::Lambda(_, _) => write!(f, "<lambda>"),
            Expr::Nil => write!(f, "()"),
            Expr::Ellipsis => write!(f, "..."),
            Expr::Dot => write!(f, "."),
            Expr::Bool(b) => {
                if *b {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            Expr::Literal(_) => write!(f, "<lit>"),
            Expr::Cons(v, e) => {
                if v.len() == 0 {
                    write!(f, "(. {:?})", e)
                } else {
                    write!(f, "(")?;
                    for item in v.iter() {
                        write!(f, "{:?} ", &item)?;
                    }
                    write!(f, ". {:?})", e)
                }
            }
        }
    }
}

pub fn corresponds(lt: &Expr, rt: &Expr) -> bool {
    macro_rules! identical {
        ( $id:tt ) => {
            match rt {
                Expr::$id => true,
                _ => false,
            }
        };
        ( $id:tt($contents:tt) ) => {
            match rt {
                Expr::$id(x) => x == $contents,
                _ => false,
            }
        };
    }
    use Expr::*;
    match lt {
        Atom(s) => identical!(Atom(s)),
        List(v) => {
            if let List(w) = rt {
                if v.len() == w.len() {
                    for i in 0..v.len() {
                        if !corresponds(&v[i], &w[i]) {
                            return false;
                        }
                    }
                    return true;
                }
            }
            false
        }
        Quote(e) => {
            if let Quote(f) = rt {
                corresponds(e, f)
            } else {
                false
            }
        }
        Quasiquote(e) => {
            if let Quasiquote(f) = rt {
                corresponds(e, f)
            } else {
                false
            }
        }
        Antiquote(e) => {
            if let Antiquote(f) = rt {
                corresponds(e, f)
            } else {
                false
            }
        }
        Integer(i) => identical!(Integer(i)),
        Float(f) => {
            if let Float(g) = rt {
                (g - f).abs() < 0.00000001
            } else {
                false
            }
        }
        String(s) => identical!(String(s)),
        Char(c) => identical!(Char(c)),
        Func(_) => false,
        Lambda(_, _) => false,
        Nil => identical!(Nil),
        Ellipsis => identical!(Ellipsis),
        Dot => identical!(Dot),
        Bool(b) => identical!(Bool(b)),
        Literal(_) => false,
        Cons(v, e) => {
            if let Cons(w, f) = rt {
                if v.len() == w.len() {
                    for i in 0..v.len() {
                        if !corresponds(&v[i], &w[i]) {
                            return false;
                        }
                    }
                }
                corresponds(e, f)
            } else {
                false
            }
        }
    }
}
