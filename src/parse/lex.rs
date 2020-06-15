use crate::parse::{ParseErr, Token, Literal};

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

#[cfg(test)]
mod test {
    use super::*;
    use crate::source;
    use crate::parse::split;
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
