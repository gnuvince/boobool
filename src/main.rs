extern crate boobool;

use boobool::errors::Result;
use boobool::parser;
use boobool::scanner;

use std::io::{self, Write};

fn main() {
    if let Err(e) = run() {
        let _ = writeln!(io::stderr(), "boobool: {}", e);
        ::std::process::exit(1);
    }
}


fn run() -> Result<()> {
    let stdin = io::stdin();
    let mut buf = String::new();
    while stdin.read_line(&mut buf).unwrap_or(0) != 0 {
        let bytes: Vec<u8> = buf.bytes().collect();
        let toks = scanner::Scanner::scan(bytes)?;
        let expr = parser::Parser::parse(toks)?;
        println!("{}", expr);
        buf.clear();
    }
    return Ok(());
}
