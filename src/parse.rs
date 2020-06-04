pub enum ParseErr {
    UnterminatedString(usize),
    IncorrectSpacing(usize),
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
    }

    #[test]
    pub fn string_split() {
        test!("\"\"" -> "\"\"");
        test!("\"abcdef\"" -> "\"abcdef\"");
        test!("(abc \"def\" ghi)" -> "(" "abc" "\"def\"" "ghi" ")");
        test!("(\"(\")" -> "(" "\"(\"" ")");
    }

    #[test]
    pub fn char_escape() {
        test!("(\\a" -> "(" "\\a");
        test!("\\'" -> "\\'");
        test!("\\\"" -> "\\\"");
        test!("(abc de (f #\\\\) #\\\") (gh #\\) (#\\i ())" -> "(" "abc" "de" "(" "f" "#\\\\" ")" "#\\\"" ")" "(" "gh" "#\\)" "(" "#\\i" "(" ")" ")");
    }
}
