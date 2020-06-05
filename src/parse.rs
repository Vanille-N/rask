#[derive(Debug, PartialEq, Eq)]
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
    let mut comment = false;
    for c in expr.chars() {
        if comment {
            if c == '\n' {
                comment = false;
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
            if len > 0 { items.push(&expr[begin..begin+len]); }
            comment = true;
            begin += 1;
            len = 0;
        } else if ['(', ')', '[', ']', ' ', '\t', '\n'].contains(&c) {
            if len > 0 { items.push(&expr[begin..begin+len]); }
            begin += len;
            if ['(', ')', '[', ']'].contains(&c) {
                items.push(&expr[begin..begin+1]);
            }
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
        if expr.len() > begin && !comment {
            items.push(&expr[begin..begin+len]);
        }
        Ok(items)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Literal {
    LoadSource,
    Exit,
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
    Char(char),
    Atom(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Literal(Literal),
}

impl std::cmp::PartialEq for Token {
    fn eq (&self, other: &Self) -> bool {
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
            }
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
            Token::Float(f) => match other {
                Token::Float(g) => (g - f).abs() < 0.00000001,
                _ => false,
            }
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
            } else if let Some(i) = s.parse::<i64>().ok() {
                Ok(Token::Integer(i))
            } else if let Some(f) = s.parse::<f64>().ok() {
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
        _ => None,
    }
}

fn is_valid_char_ident(c: char) -> bool {
    '0' <= c && c <= '9'
    || 'a' <= c && c <= 'z'
    || 'A' <= c && c <= 'Z'
    || "+-?!~/_%=*<>$|^@&".contains(c)
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

    #[test]
    pub fn comments() {
        test!(";abc\ndef;a" -> "def");
        test!(";;; x y z \n a b c \n ;e" -> "a" "b" "c");
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
        ( $id:tt ) => { assert!(lex($id).is_ok()) }
    }

    macro_rules! fail {
        ( $id:tt ) => { assert!(lex($id).is_err()) }
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
                Ok(_) => {},
            }
        }
    }

    #[test]
    pub fn real_program() {
        let sp = split(PROG).ok().unwrap();
        for s in sp.into_iter() {
            assert_lexable!(s);
        }
    }
}

pub const PROG: &str = "
;;; make-matrix creates a matrix (a vector of vectors).
(define make-matrix
  (lambda (rows columns)
    (do ((m (make-vector rows))
         (i 0 (+ i 1)))
        ((= i rows) m)
        (vector-set! m i (make-vector columns)))))

;;; matrix? checks to see if its argument is a matrix.
;;; It isn't foolproof, but it's generally good enough.
(define matrix?
  (lambda (x)
    (and (vector? x)
         (> (vector-length x) 0)
         (vector? (vector-ref x 0)))))

;; matrix-rows returns the number of rows in a matrix.
(define matrix-rows
   (lambda (x)
      (vector-length x)))

;; matrix-columns returns the number of columns in a matrix.
(define matrix-columns
   (lambda (x)
      (vector-length (vector-ref x 0))))

;;; matrix-ref returns the jth element of the ith row.
(define matrix-ref
  (lambda (m i j)
    (vector-ref (vector-ref m i) j)))

;;; matrix-set! changes the jth element of the ith row.
(define matrix-set!
  (lambda (m i j x)
    (vector-set! (vector-ref m i) j x)))

;;; mul is the generic matrix/scalar multiplication procedure
(define mul
  (lambda (x y)
    ;; mat-sca-mul multiplies a matrix by a scalar.
    (define mat-sca-mul
       (lambda (m x)
          (let* ((nr (matrix-rows m))
                 (nc (matrix-columns m))
                 (r  (make-matrix nr nc)))
             (do ((i 0 (+ i 1)))
                 ((= i nr) r)
                 (do ((j 0 (+ j 1)))
                     ((= j nc))
                     (matrix-set! r i j
                        (* x (matrix-ref m i j))))))))

    ;; mat-mat-mul multiplies one matrix by another, after verifying
    ;; that the first matrix has as many columns as the second
    ;; matrix has rows.
    (define mat-mat-mul
       (lambda (m1 m2)
          (let* ((nr1 (matrix-rows m1))
                 (nr2 (matrix-rows m2))
                 (nc2 (matrix-columns m2))
                 (r   (make-matrix nr1 nc2)))
             (if (not (= (matrix-columns m1) nr2))
                 (match-error m1 m2))
             (do ((i 0 (+ i 1)))
                 ((= i nr1) r)
                 (do ((j 0 (+ j 1)))
                     ((= j nc2))
                     (do ((k 0 (+ k 1))
                          (a 0
                             (+ a
                                (* (matrix-ref m1 i k)
                                   (matrix-ref m2 k j)))))
                         ((= k nr2)
                          (matrix-set! r i j a))))))))

   ;; type-error is called to complain when mul receives an invalid
   ;; type of argument.
    (define type-error
       (lambda (what)
          (error 'mul
             \"~s is not a number or matrix\"
             what)))

    ;; match-error is called to complain when mul receives a pair of
    ;; incompatible arguments.
    (define match-error
       (lambda (what1 what2)
          (error 'mul
             \"~s and ~s are incompatible operands\"
             what1
             what2)))

    ;; body of mul; dispatch based on input types
    (cond
      ((number? x)
       (cond
         ((number? y) (* x y))
         ((matrix? y) (mat-sca-mul y x))
         (else (type-error y))))
      ((matrix? x)
       (cond
         ((number? y) (mat-sca-mul x y))
         ((matrix? y) (mat-mat-mul x y))
         (else (type-error y))))
      (else (type-error x)))))
";
