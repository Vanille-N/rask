use std::env;
use std::process;

use rask::parse::{split, ParseErr};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Not enough arguments");
        process::exit(1);
    }
    match &args[1][..] {
        "parse" => {
            for arg in args.iter().skip(2) {
                match split(&arg) {
                    Ok(v) => println!("{:?}", &v),
                    Err(e) => {
                        let n = arg.len();
                        match e {
                            ParseErr::UnterminatedString(pos) => {
                                eprintln!("Unterminated string literal");
                                eprintln!(
                                    "  Found in expression {}...",
                                    &arg[0..10.min(arg.len())]
                                );
                                eprintln!(
                                    "  At position {}: {}...",
                                    pos,
                                    &arg[pos..(pos + 10).min(n)]
                                );
                            }
                            ParseErr::IncorrectSpacing(pos) => {
                                eprintln!("Incorrect spacing between disctinct elements");
                                eprintln!("  Found in expression {}...", &arg[0..10.min(n)]);
                                eprintln!(
                                    "  At position {}: {}...",
                                    pos,
                                    &arg[pos..(pos + 10).min(n)]
                                );
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
                        }
                    }
                }
            }
        }
        _ => (),
    }
}
