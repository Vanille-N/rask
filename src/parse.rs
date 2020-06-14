use chainmap::ChainMap;
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
use std::{cmp, fmt};

pub fn source(fname: &str) -> Option<String> {
    if let Ok(mut file) = File::open(fname.to_owned() + ".scm") {
        let mut contents = String::new();
        match file.read_to_string(&mut contents) {
            Ok(_) => Some(contents),
            Err(_) => None,
        }
    } else {
        None
    }
}

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

#[derive(Debug, PartialEq, Eq)]
pub enum EvalErr {}

pub fn split(expr: &str) -> Result<Vec<&str>, ParseErr> {
    let mut begin = 0;
    let mut len = 0;
    let mut items = Vec::new();
    let mut string = false;
    let mut escape = false;
    let mut line_comment = false;
    let mut comment = 0;
    while begin + len < expr.len() {
        let c = expr[begin + len..begin + len + 1].chars().next().unwrap();
        if line_comment {
            if c == '\n' {
                line_comment = false;
            }
            begin += 1;
        } else if comment > 0 {
            if begin + len + 1 < expr.len() && &expr[begin + len..begin + len + 2] == "|#" {
                comment -= 1;
                begin += 1;
            } else if begin + len + 1 < expr.len() && &expr[begin + len..begin + len + 2] == "#|" {
                comment += 1;
                begin += 1;
            }
            begin += 1;
        } else if string {
            if c == '"' {
                string = false;
            }
            len += 1;
        } else if escape {
            len += 1;
            escape = false;
        } else if c == '"' && !string {
            string = true;
            len += 1;
        } else if c == '\\' {
            escape = true;
            len += 1;
        } else if c == ';' {
            if len > 0 {
                items.push(&expr[begin..begin + len]);
            }
            line_comment = true;
            begin += len + 1;
            len = 0;
        } else if begin + len + 1 < expr.len() && &expr[begin + len..begin + len + 2] == "#|" {
            if len > 0 {
                items.push(&expr[begin..begin + len]);
            }
            comment = 1;
            begin += len + 2;
            len = 0;
        } else if begin + len + 1 < expr.len() && &expr[begin + len..begin + len + 2] == "|#" {
            return Err(ParseErr::NoCommentStart);
        } else if "()[] \t\n".contains(c) {
            if len > 0 {
                items.push(&expr[begin..begin + len]);
            }
            begin += len;
            if "()[]".contains(c) {
                items.push(&expr[begin..begin + 1]);
            }
            len = 0;
            begin += 1;
        } else {
            len += 1;
        }
    }
    if string {
        if &expr[begin..begin + 1] == "\"" {
            Err(ParseErr::UnterminatedString(begin))
        } else {
            Err(ParseErr::IncorrectSpacing(begin))
        }
    } else if comment > 0 {
        Err(ParseErr::UnterminatedComment)
    } else {
        if expr.len() > begin && !line_comment {
            items.push(&expr[begin..begin + len]);
        }
        Ok(items)
    }
}

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

pub fn lex(item: &str) -> Result<Token, ParseErr> {
    match item {
        "(" => Ok(Token::OpenParen),
        ")" => Ok(Token::CloseParen),
        "[" => Ok(Token::OpenBrace),
        "]" => Ok(Token::CloseBrace),
        "'" => Ok(Token::Quote),
        "`" => Ok(Token::Quasiquote),
        "," => Ok(Token::Antiquote),
        "." => Ok(Token::Dot),
        "..." => Ok(Token::Ellipsis),
        s => {
            let chars = s.chars().collect::<Vec<_>>();
            if chars[0] == '#' {
                if chars.len() == 1 {
                    Err(ParseErr::LoneNumbersign)
                } else if chars[1] == '\\' {
                    let c = chars.iter().skip(2).collect::<String>();
                    match verify_char(&c[..]) {
                        None => Err(ParseErr::InvalidChar(c)),
                        Some(chr) => Ok(Token::Char(chr)),
                    }
                } else if item == "#t" {
                    Ok(Token::Bool(true))
                } else if item == "#f" {
                    Ok(Token::Bool(false))
                } else {
                    let l = chars[1..].iter().collect::<String>();
                    match verify_literal(&l[..]) {
                        None => Err(ParseErr::InvalidLiteral(l)),
                        Some(lit) => Ok(Token::Literal(lit)),
                    }
                }
            } else if chars[0] == '"' {
                // Correct string ending already verified
                Ok(Token::String(
                    chars[1..chars.len() - 1].iter().collect::<String>(),
                ))
            } else if let Ok(i) = s.parse::<i64>() {
                Ok(Token::Integer(i))
            } else if let Ok(f) = s.parse::<f64>() {
                Ok(Token::Float(f))
            } else if let Some(id) = verify_identifier(&s) {
                Ok(Token::Atom(id))
            } else {
                Err(ParseErr::InvalidIdent(String::from(s)))
            }
        }
    }
}

pub fn distribute_lex(s: Vec<&str>) -> Result<Vec<Token>, ParseErr> {
    let mut tokens = Vec::new();
    for item in s {
        tokens.push(lex(item)?);
    }
    Ok(tokens)
}

fn verify_char(s: &str) -> Option<char> {
    if s.len() == 1 {
        Some(s.chars().next().unwrap())
    } else {
        match s {
            "nul" => Some('\x00'),
            "alarm" => Some('\x07'),
            "backspace" => Some('\x08'),
            "tab" => Some('\x09'),
            "linefeed" => Some('\x0A'),
            "newline" => Some('\x0A'),
            "vtab" => Some('\x0B'),
            "page" => Some('\x0C'),
            "return" => Some('\x0D'),
            "esc" => Some('\x1B'),
            "space" => Some('\x20'),
            "delete" => Some('\x7F'),
            _ => None,
        }
    }
}

fn verify_literal(s: &str) -> Option<Literal> {
    match s {
        "load" => Some(Literal::LoadSource),
        "exit" => Some(Literal::Exit),
        "show" => Some(Literal::Show),
        _ => None,
    }
}

fn is_valid_char_ident(c: char) -> bool {
    '0' <= c && c <= '9'
        || 'a' <= c && c <= 'z'
        || 'A' <= c && c <= 'Z'
        || ":!$%&*+./<=>?@^|-_~".contains(c)
}

fn verify_identifier(s: &str) -> Option<String> {
    for c in s.chars() {
        if !is_valid_char_ident(c) {
            return None;
        }
    }
    Some(String::from(s))
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

pub fn parse(tokens: Vec<Token>) -> Result<Expr, ParseErr> {
    parse_helper(&tokens, &mut 0)
}

pub fn close_separator(op: &Token) -> Token {
    match op {
        Token::OpenParen => Token::CloseParen,
        Token::OpenBrace => Token::CloseBrace,
        _ => panic!("{:?} is not closable", op),
    }
}

pub fn parse_helper(tokens: &[Token], idx: &mut usize) -> Result<Expr, ParseErr> {
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
                let expr = parse_helper(tokens, idx)?;
                if let Expr::Dot = expr {
                    if dot_seen {
                        return Err(ParseErr::InvalidCons);
                    } else {
                        dot_seen = true;
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
            *idx += 1;
            if dot_seen {
                let expr = parse_helper(tokens, idx)?;
                if *idx < tokens.len() && tokens[*idx + 1] == cl {
                    Ok(Expr::Cons(v, Box::new(expr)))
                } else {
                    Err(ParseErr::InvalidCons)
                }
            } else {
                Ok(Expr::List(v))
            }
        }
        Token::CloseParen => Err(ParseErr::MismatchedCloseParen),
        Token::CloseBrace => Err(ParseErr::MismatchedCloseBrace),
        Token::Quote => Ok(Expr::Quote(Box::new(parse_helper(tokens, idx)?))),
        Token::Quasiquote => Ok(Expr::Quasiquote(Box::new(parse_helper(tokens, idx)?))),
        Token::Antiquote => Ok(Expr::Antiquote(Box::new(parse_helper(tokens, idx)?))),
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

fn corresponds(lt: &Expr, rt: &Expr) -> bool {
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

#[cfg(test)]
mod test_split {
    macro_rules! test {
        ( $input:tt -> $( $output:tt )* ) => {
            assert_eq!(split($input).ok().unwrap(), vec![ $( $output ),* ] as Vec<&str>);
        }
    }

    use super::*;
    #[test]
    pub fn simple_splits() {
        test!("(f a b)" -> "(" "f" "a" "b" ")");
        test!("()()()" -> "(" ")" "(" ")" "(" ")");
        test!("((())" -> "(" "(" "(" ")" ")");
        test!(") abc(" -> ")" "abc" "(");
        test!("abc de (a cfg (b d)) )" -> "abc" "de" "(" "a" "cfg" "(" "b" "d" ")" ")" ")");
        test!("+-123 // <e> (%1 11>1) ?~" -> "+-123" "//" "<e>" "(" "%1" "11>1" ")" "?~");
        test!("(f args ...)" -> "(" "f" "args" "..." ")");
    }

    #[test]
    pub fn string_split() {
        test!("\"\"" -> "\"\"");
        test!("\"abcdef\"" -> "\"abcdef\"");
        test!("(abc \"def\" ghi)" -> "(" "abc" "\"def\"" "ghi" ")");
        test!("(\"(\")" -> "(" "\"(\"" ")");
    }

    #[test]
    pub fn char_escape_and_literals() {
        test!("(\\a" -> "(" "\\a");
        test!("\\'" -> "\\'");
        test!("\\\"" -> "\\\"");
        test!("(abc de (f #\\\\) #\\\") (gh #\\) (#\\i ())" -> "(" "abc" "de" "(" "f" "#\\\\" ")" "#\\\"" ")" "(" "gh" "#\\)" "(" "#\\i" "(" ")" ")");
        test!("#true #f #\\t #\\em #\\tab" -> "#true" "#f" "#\\t" "#\\em" "#\\tab");
    }

    #[test]
    pub fn comments() {
        test!(";abc\ndef;a" -> "def");
        test!(";;; x y z \n a b c \n ;e" -> "a" "b" "c");
        test!("this is an#|~~ inline |#comment" -> "this" "is" "an" "comment");
        test!("[.(.#|.|#.].)" -> "[" "." "(" "." "." "]" "." ")");
        test!("#|comment|#" -> );
        test!("a#| #| c|# d|# f" -> "a" "f");
    }
}

#[cfg(test)]
mod test_lex {
    use super::*;

    macro_rules! test {
        ( $input:tt -> +$( $output:tt )+ ) => {
            assert_eq!(lex($input).ok().unwrap(), Token::$( $output )+)
        };
        ( $input:tt -> -$( $output:tt )+ ) => {
            assert_eq!(lex($input).err().unwrap(), ParseErr::$( $output )+)
        }
    }

    #[test]
    pub fn separators() {
        test!("(" -> +OpenParen);
        test!(")" -> +CloseParen);
        test!("[" -> +OpenBrace);
        test!("]" -> +CloseBrace);
        test!("." -> +Dot);
        test!("'" -> +Quote);
        test!("," -> +Antiquote);
        test!("`" -> +Quasiquote);
        test!("..." -> +Ellipsis);
    }

    #[test]
    pub fn literals_and_chars() {
        test!("#" -> -LoneNumbersign);
        test!("#\\" -> -InvalidChar(String::from("")));
        test!("#\\a" -> +Char('a'));
        test!("#\\\\" -> +Char('\\'));
        test!("#\\)" -> +Char(')'));
        test!("#\\abc" -> -InvalidChar(String::from("abc")));
        test!("#\\tab" -> +Char('\t'));
        test!("#\\newline" -> +Char('\n'));
        test!("#newline" -> -InvalidLiteral(String::from("newline")));
        test!("#t" -> +Bool(true));
        test!("#f" -> +Bool(false));
        test!("#load" -> +Literal(Literal::LoadSource));
        test!("#exit" -> +Literal(Literal::Exit));
        test!("#\\xy" -> -InvalidChar(String::from("xy")));
    }

    #[test]
    pub fn numerics_and_strings() {
        test!("1234" -> +Integer(1234));
        test!("-125" -> +Integer(-125));
        test!("1253.7" -> +Float(1253.7));
        test!("12e-1" -> +Float(1.2));
        test!("0.5" -> +Float(0.5));
        test!(".5" -> +Float(0.5));
        test!("0." -> +Float(0.0));
        test!("\"abc\"" -> +String(String::from("abc")));
        test!("\"aaa\\\"bbb\"" -> +String(String::from("aaa\\\"bbb")));
        test!("abc" -> +Atom(String::from("abc")));
    }

    macro_rules! ident {
        ( $id:tt ) => {
            assert!(lex($id).is_ok())
        };
    }

    macro_rules! fail {
        ( $id:tt ) => {
            assert!(lex($id).is_err())
        };
    }

    #[test]
    pub fn identifiers() {
        ident!("abc");
        ident!("++1");
        ident!("~00000-/71o%");
        ident!("-_-");
        ident!("^1@&$");
        ident!("12+");
        ident!("<=?");
        fail!("{abc");
        fail!("x\\");
        fail!("yzabc#");
        fail!("|)");
    }

    macro_rules! assert_lexable {
        ( $x:tt ) => {
            match split($x) {
                Err(e) => panic!("{:?} failed to lex properly: returned {:?}", $x, e),
                Ok(_) => {}
            }
        };
    }

    #[test]
    pub fn real_program() {
        let contents = source("assets/matrix").unwrap();
        let sp = split(&contents[..]).ok().unwrap();
        for s in sp.into_iter() {
            assert_lexable!(s);
        }
    }
}

#[cfg(test)]
mod test_parse {
    use super::*;
    macro_rules! check {
        ( $s:tt -> +$e:expr ) => {
            let sp = split($s);
            if let Err(e) = sp {
                panic!("Failed to split: {:?}", e);
            }
            let tokens = distribute_lex(sp.ok().unwrap());
            if let Err(e) = tokens {
                panic!("Failed to lex: {:?}", e);
            }
            let lt = parse(tokens.ok().unwrap());
            if let Err(e) = lt {
                panic!("Failed to parse: {:?}", e);
            }
            let lt = lt.ok().unwrap();
            if !corresponds(&lt, &$e) {
                panic!(
                    "Parsing mistake:\n    {:?} is not the same as \n    {:?}",
                    lt,
                    $e
                );
            }
        };
        ( $s:tt -> -$e:expr ) => {
            let tokens = lex(split(s));
            let lt = parse(tokens).err().unwrap();
            assert_eq!(lt, ParseErr::$e);
        };
    }
}
