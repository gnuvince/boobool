extern crate boobool;

use boobool::errors::Result;
use boobool::parser;
use boobool::scanner;
use boobool::typechecker;
use boobool::types::{Type, Symtable};

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
    let st = make_symtable();
    while stdin.read_line(&mut buf).unwrap_or(0) != 0 {
        let bytes: Vec<u8> = buf.bytes().collect();
        let toks = scanner::Scanner::scan(bytes)?;
        let expr = parser::Parser::parse(toks)?;
        let texpr = typechecker::tc_expr(expr, &st)?;
        println!("{}", texpr);
        buf.clear();
    }
    return Ok(());
}


fn make_symtable() -> Symtable {
    let mut st = Symtable::new();
    st.add("width".to_string(), Type::Int);
    st.add("height".to_string(), Type::Int);
    return st;
}
