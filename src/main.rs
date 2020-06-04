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
                let v = split(&args[i]);
                println!("{:?}", &v);
            }
        }
        _ => (),
    }
}

fn split(expr: &str) -> Vec<&str> {
    let mut begin = 0;
    let mut len = 0;
    let mut items = Vec::new();
    for c in expr.chars() {
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
    items
}
