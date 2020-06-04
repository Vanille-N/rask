use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Not enough arguments");
        process::exit(1);
    }
    match &args[1][..] {
        "parse" => {
            for i in 2..args.len() {
                match split(&args[i]) {
                    Ok(v) => println!("{:?}", &v),
                    Err(e) => {
                        match e {
                            ParseErr::UnterminatedString(pos) => {
                                eprintln!("Unterminated string literal");
                                eprintln!("  Found in expression {}...", &args[i][0..10.min(args[i].len())]);
                                eprintln!("  At position {}: {}...", pos, &args[i][pos..(pos+10).min(args[i].len())]);
                            }
                        }
                    }
                }
            }
        }
        _ => (),
    }
}

enum ParseErr {
    UnterminatedString(usize),
}

fn split(expr: &str) -> Result<Vec<&str>, ParseErr> {
    let mut begin = 0;
    let mut len = 0;
    let mut items = Vec::new();
    let mut string = false;
    for c in expr.chars() {
        if c == '"' && !string {
            string = true;
            len += 1;
            continue;
        }
        if string {
            if c == '"' {
                string = false;
            }
            len += 1;
            continue;
        }
        if ['(', ')', '[', ']', ' '].contains(&c) {
            if len > 0 { items.push(&expr[begin..begin+len]); }
            begin += len;
            if c != ' ' { items.push(&expr[begin..begin+1]); }
            len = 0;
            begin += 1;
        } else {
            len += 1;
        }
    }
    if begin == expr.len() {
        Ok(items)
    } else {
        Err(ParseErr::UnterminatedString(begin))
    }
}
