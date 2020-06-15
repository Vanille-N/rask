use std::fs::File;
use std::io::Read;

pub mod parse;

pub fn source(fname: &str) -> Option<String> {
    if let Ok(mut file) = File::open(fname.to_owned() + ".scm") {
        let mut contents = String::new();
        match file.read_to_string(&mut contents) {
            Ok(_) => Some(contents),
            Err(_) => None,
        }
    } else {
        None
    }
}
