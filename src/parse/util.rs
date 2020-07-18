use chainmap::ChainMap;
use std::rc::Rc;
use std::{cmp, fmt};

#[derive(Debug)]
pub enum ParseErr {
    UnterminatedString(usize),
    IncorrectSpacing(usize),
    LoneNumbersign,
    InvalidChar(String),
    InvalidLiteral(String),
    InvalidIdent(String),
    UnterminatedComment,
    NoCommentStart,
    MismatchedOpenParen(usize),
    MismatchedCloseParen(usize),
    MismatchedOpenBrace(usize),
    MismatchedCloseBrace(usize),
    Unfinished,
    InvalidCons(usize),
    InvalidVec(usize),
}

#[cfg_attr(tarpaulin, skip)]
impl cmp::PartialEq for ParseErr {
    fn eq(&self, other: &Self) -> bool {
        macro_rules! identical {
            ( $id:tt ) => {
                match other {
                    ParseErr::$id => true,
                    _ => false,
                }
            };
            ( $id:tt(_) ) => {
                match other {
                    ParseErr::$id(_) => true,
                    _ => false,
                }
            };
            ( $id:tt($contents:tt) ) => {
                match other {
                    ParseErr::$id(x) => x == $contents,
                    _ => false,
                }
            };
        }
        match self {
            ParseErr::UnterminatedString(_) => identical!(UnterminatedString(_)),
            ParseErr::IncorrectSpacing(_) => identical!(IncorrectSpacing(_)),
            ParseErr::LoneNumbersign => identical!(LoneNumbersign),
            ParseErr::InvalidChar(s) => identical!(InvalidChar(s)),
            ParseErr::InvalidLiteral(l) => identical!(InvalidLiteral(l)),
            ParseErr::InvalidIdent(s) => identical!(InvalidIdent(s)),
            ParseErr::UnterminatedComment => identical!(UnterminatedComment),
            ParseErr::NoCommentStart => identical!(NoCommentStart),
            ParseErr::MismatchedOpenParen(_) => identical!(MismatchedOpenParen(_)),
            ParseErr::MismatchedCloseParen(_) => identical!(MismatchedCloseParen(_)),
            ParseErr::MismatchedOpenBrace(_) => identical!(MismatchedOpenBrace(_)),
            ParseErr::MismatchedCloseBrace(_) => identical!(MismatchedCloseBrace(_)),
            ParseErr::Unfinished => identical!(Unfinished),
            ParseErr::InvalidCons(_) => identical!(InvalidCons(_)),
            ParseErr::InvalidVec(_) => identical!(InvalidVec(_)),
        }
    }
}

impl cmp::Eq for ParseErr {}

#[derive(Debug, Clone)]
pub enum Token {
    OpenParen,
    CloseParen,
    VecParen,
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
}

#[cfg_attr(tarpaulin, skip)]
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
            Token::VecParen => identical!(VecParen),
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
            Token::Ellipsis => identical!(Ellipsis),
            Token::Float(f) => match other {
                Token::Float(g) => (g - f).abs() < 0.00000001,
                _ => false,
            },
        }
    }
}

pub enum Expr {
    Atom(Rc<String>),
    List(Rc<Vec<Rc<Expr>>>),
    Vec(Rc<Vec<Rc<Expr>>>),
    Quote(Rc<Expr>),
    Quasiquote(Rc<Expr>),
    Antiquote(Rc<Expr>),
    Integer(i64),
    Float(f64),
    String(Rc<String>),
    Char(char),
    Func(Func),
    Ellipsis,
    Dot,
    Bool(bool),
    Cons(Rc<Vec<Rc<Expr>>>, Rc<Expr>),
}

pub type Func = Rc<dyn Fn(&[Rc<Expr>], &mut Envt) -> Result<Rc<Expr>, crate::exec::EvalErr>>;

pub type Envt = ChainMap<String, Rc<Expr>>;

#[cfg_attr(tarpaulin, skip)]
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
            Expr::Ellipsis => write!(f, "..."),
            Expr::Dot => write!(f, "."),
            Expr::Bool(b) => {
                if *b {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
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
            Expr::Vec(v) => {
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
        Ellipsis => identical!(Ellipsis),
        Dot => identical!(Dot),
        Bool(b) => identical!(Bool(b)),
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
        Vec(v) => {
            if let Vec(w) = rt {
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
    }
}
