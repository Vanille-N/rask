pub enum ParseErr {
    UnterminatedString(usize),
    IncorrectSpacing(usize),
    LoneNumbersign,
    InvalidChar(String),
    InvalidLiteral(String),
    InvalidIdent(String),
}

pub fn split(expr: &str) -> Result<Vec<&str>, ParseErr> {
    let mut begin = 0;
    let mut len = 0;
    let mut items = Vec::new();
    let mut string = false;
    let mut escape = false;
    for c in expr.chars() {
        if string {
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
        } else if ['(', ')', '[', ']', ' '].contains(&c) {
            if len > 0 { items.push(&expr[begin..begin+len]); }
            begin += len;
            if c != ' ' { items.push(&expr[begin..begin+1]); }
            len = 0;
            begin += 1;
        } else {
            len += 1;
        }
    }
    if string {
        if &expr[begin..begin+1] == "\"" {
            Err(ParseErr::UnterminatedString(begin))
        } else {
            Err(ParseErr::IncorrectSpacing(begin))
        }
    } else {
        if expr.len() > begin {
            items.push(&expr[begin..begin+len]);
        }
        Ok(items)
    }
}

enum Literal {
    LoadSource,
    Exit,
}

enum Token {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Dot,
    Char(char),
    Atom(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Literal(Literal),
}

fn lex(item: &str) -> Result<Token, ParseErr> {
    match item {
        "(" => Ok(Token::OpenParen),
        ")" => Ok(Token::CloseParen),
        "[" => Ok(Token::OpenBrace),
        "]" => Ok(Token::CloseBrace),
        "." => Ok(Token::Dot),
        s => {
            let chars = s.chars().collect::<Vec<_>>();
            if chars[0] == '#' {
                if chars.len() == 1 {
                    Err(ParseErr::LoneNumbersign)
                } else if chars[1] == '\\' {
                    let c = chars[2..].iter().collect::<String>();
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
            } else if chars[0] == '"' { // Correct string ending already verified
                Ok(Token::String(chars[1..chars.len()-1].iter().collect::<String>()))
            } else if let Some(i) = verify_integer(&s) {
                Ok(Token::Integer(i))
            } else if let Some(f) = verify_float(&s) {
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

fn verify_integer(s: &str) -> Option<i64> {
    unimplemented!()
}

fn verify_float(s: &str) -> Option<f64> {
    unimplemented!()
}

fn verify_literal(s: &str) -> Option<Literal> {
    unimplemented!()
}

fn verify_identifier(s: &str) -> Option<String> {
    unimplemented!()
}

#[cfg(test)]
mod test_split {
    macro_rules! test {
        ( $input:tt -> $( $output:tt )* ) => {
            assert_eq!(split($input).ok(), Some(vec![ $( $output ),* ]));
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
        test!("[.(.{.}.].)" -> "[" "." "(" ".{.}." "]" "." ")");
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
}
