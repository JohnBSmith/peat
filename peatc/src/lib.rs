
use std::fs::File;
use std::io;
use std::io::Read;

mod parser;

pub fn read_file(path: &str) -> Result<String,io::Error> {
    let mut file = File::open(path)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    return Ok(input);
}

pub fn compile(input: &str) {
    let _t = match parser::parse(input) {
        Ok(value) => value,
        Err(e) => {
            println!("{}",e.text);
            return;
        }
    };
}
