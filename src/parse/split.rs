use crate::parse::ParseErr;

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
        } else if c == '\'' {
            begin += 1;
            items.push("'");
        } else if c == ',' {
            begin += 1;
            items.push(",");
        } else if c == '`' {
            begin += 1;
            items.push("`");
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

#[cfg(test)]
mod test {
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

    #[test]
    pub fn quotes() {
        test!("'a" -> "'" "a");
        test!("`(ab ,c d)" -> "`" "(" "ab" "," "c" "d" ")");
        test!("ab `c" -> "ab" "`" "c");
    }
}
