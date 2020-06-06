use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

pub fn source(fname: &str) -> Option<String> {
    if let Ok(mut file) = File::open(fname.to_owned() + ".scm") {
        let mut contents = String::new();
        file.read_to_string(&mut contents);
        Some(contents)
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

impl std::cmp::PartialEq for Token {
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
    Nil,
    Ellipsis,
    Atom(String),
    List(Vec<Expr>),
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Fn(Func),
    Lambda(Func, Envt),
    Literal(Literal),
    Cons(Vec<Expr>, Box<Expr>),
}

pub struct Func(Box<dyn Fn(Expr) -> Result<Expr, EvalErr>>);

pub struct Envt(HashMap<String, Expr>);

pub fn parse(tokens: Vec<Token>) -> Expr {
    unimplemented!()
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
        test!("\"abc\"" -> +String(String::from("abc")));
        test!("\"aaa\\\"bbb\"" -> +String(String::from("aaa\\\"bbb")));
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
