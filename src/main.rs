use std::env;
use std::process;

use rask::parse::{ParseErr, split};

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
                            ParseErr::IncorrectSpacing(pos) => {
                                eprintln!("Incorrect spacing between disctinct elements");
                                eprintln!("  Found in expression {}...", &args[i][0..10.min(args[i].len())]);
                                eprintln!("  At position {}: {}...", pos, &args[i][pos..(pos+10).min(args[i].len())]);
                            }
                            ParseErr::LoneNumbersign => {
                                eprintln!("Number sign not followed by an interpreter directive nor a character");
                                eprintln!("  Found in expression {}...", &args[i][0..10.min(args[i].len())]);
                            }
                            ParseErr::InvalidChar(chr) => {
                                eprintln!("Invalid character literal");
                                eprintln!("  Found in expression {}...", &args[i][0..10.min(args[i].len())]);
                                eprintln!("  {} is not recognized", chr);
                            }
                            ParseErr::InvalidLiteral(lit) => {
                                eprintln!("Not a valid interpreter directive");
                                eprintln!("  Found in expression {}...", &args[i][0..10.min(args[i].len())]);
                                eprintln!("  {} is not recognized", lit);
                            }
                            ParseErr::InvalidIdent(id) => {
                                eprintln!("Not a valid identifier directive");
                                eprintln!("  Found in expression {}...", &args[i][0..10.min(args[i].len())]);
                                eprintln!("  {} contains invalid characters for an identifier", id);
                            }
                        }
                    }
                }
            }
        }
        _ => (),
    }
}
