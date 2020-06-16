mod build;
mod lex;
mod split;
mod util;

pub use build::build;
pub use lex::distribute_lex as lex;
pub use split::split;
pub use util::*;

#[cfg(test)]
mod integrate {
    const ASSETS: [&str; 9] = [
        "sort",
        "set-construct",
        "word-count",
        "printer",
        "interprete",
        "unification",
        "timer",
        "sprintf",
        "matrix",
    ];
    use super::*;
    use crate::source;

    #[test]
    fn read_sources() {
        for file in ASSETS.iter() {
            let prog = source(&("assets/".to_owned() + *file)).unwrap();
            let symbols = split(&prog[..]);
            if let Err(e) = symbols {
                panic!("Could not split {} properly: {:?}", file, e);
            }
            let symbols = symbols.ok().unwrap();
            let tokens = lex(symbols);
            if let Err(e) = tokens {
                panic!("Could not tokenize {} properly: {:?}", file, e);
            }
            let tokens = tokens.ok().unwrap();
            let exprs = build(tokens.clone());
            for expr in exprs.iter() {
                if let Err(e) = expr {
                    match e {
                        ParseErr::MismatchedOpenBrace(n)
                        | ParseErr::MismatchedOpenParen(n)
                        | ParseErr::MismatchedCloseBrace(n)
                        | ParseErr::MismatchedCloseParen(n) => panic!(
                            "Could not build {} properly: {:?}\nContext: {:?}",
                            file,
                            e,
                            &tokens[n - 5..n + 5]
                        ),
                        e => panic!("Could not build {} properly: {:?}", file, e),
                    }
                }
            }
        }
    }
}
