use std::env;
use std::process;

use rask::parse::{lex, parse, split, ParseErr};

#[cfg_attr(tarpaulin, skip)]
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Not enough arguments");
        process::exit(1);
    }
    #[allow(clippy::single_match)]
    match &args[1][..] {
        "split" => {
            for arg in args.iter().skip(2) {
                match split(&arg) {
                    Ok(v) => println!("{:?}", &v),
                    Err(e) => errmsg_parse(&arg, e),
                }
            }
        }
        "lex" => {
            for arg in args.iter().skip(2) {
                let tokens = match split(&arg) {
                    Ok(v) => v,
                    Err(e) => {
                        errmsg_parse(&arg, e);
                        continue;
                    }
                };
                match lex(&tokens) {
                    Ok(v) => println!("{:?}", &v),
                    Err(e) => errmsg_parse(&arg, e),
                }
            }
        }
        "parse" => {
            for arg in args.iter().skip(2) {
                for elem in parse(&arg) {
                    match elem {
                        Ok(v) => println!("{:?}", &v),
                        Err(e) => errmsg_parse(&arg, e),
                    }
                }
            }
        }
        _ => (),
    }
}

#[cfg_attr(tarpaulin, skip)]
fn errmsg_parse(arg: &str, e: ParseErr) {
    let n = arg.len();
    match e {
        ParseErr::UnterminatedString(pos) => {
            eprintln!("Unterminated string literal");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  At position {}: {}...", pos, &arg[pos..(pos + 10).min(n)]);
        }
        ParseErr::IncorrectSpacing(pos) => {
            eprintln!("Incorrect spacing between disctinct elements");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  At position {}: {}...", pos, &arg[pos..(pos + 10).min(n)]);
        }
        ParseErr::LoneNumbersign => {
            eprintln!("Number sign not followed by an interpreter directive nor a character");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
        }
        ParseErr::InvalidChar(chr) => {
            eprintln!("Invalid character literal");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  {} is not recognized", chr);
        }
        ParseErr::InvalidLiteral(lit) => {
            eprintln!("Not a valid interpreter directive");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  {} is not recognized", lit);
        }
        ParseErr::InvalidIdent(id) => {
            eprintln!("Not a valid identifier directive");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  {} contains invalid characters for an identifier", id);
        }
        ParseErr::UnterminatedComment => {
            eprintln!("Found the start of an inline comment, but no matching terminator");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
        }
        ParseErr::NoCommentStart => {
            eprintln!("Found an inline-comment terminator, but no matching beginning");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
        }
        ParseErr::MismatchedOpenBrace(idx) => {
            eprintln!("Found an opening bracket `[` with no corresponding `]`");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  Token identifier: {}", idx);
        }
        ParseErr::MismatchedCloseBrace(idx) => {
            eprintln!("Did not expect closing bracket `]` without matching `[` first");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  Token identifier: {}", idx);
        }
        ParseErr::MismatchedOpenParen(idx) => {
            eprintln!("Found an opening parenthesis `(` with no corresponding `)`");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  Token identifier: {}", idx);
        }
        ParseErr::MismatchedCloseParen(idx) => {
            eprintln!("Did not expect closing parenthesis `)` without matching `(` first");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  Token identifier: {}", idx);
        }
        ParseErr::Unfinished => {
            eprintln!("Unexpected end of expression");
            eprintln!("  Check your usage of quotes and dots in particular");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
        }
        ParseErr::InvalidCons(idx) => {
            eprintln!("Cannot make a cons of anything other than a list and an expression");
            eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
            eprintln!("  Token identifier: {}", idx);
        }
    }
}
