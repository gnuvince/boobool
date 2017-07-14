use std::collections::HashMap;
use std::fmt;

use errors::{Error, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Int,
    Float,
    Str,
    List(Box<Type>),
    Func(Vec<Type>, Box<Type>),
}


impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Str => write!(f, "str"),
            Type::List(ref t) => write!(f, "List({})", t),
            Type::Func(_, _) => {
                write!(f, "function")
            }
        }
    }
}


pub struct Symtable {
    symbols: HashMap<String, Type>
}


impl Symtable {
    pub fn new() -> Symtable {
        return Symtable { symbols: HashMap::with_capacity(64) };
    }


    pub fn add(&mut self, var: String, ty: Type) {
        self.symbols.insert(var, ty);
    }


    pub fn get(&self, var: &str) -> Result<Type> {
        let ty = self.symbols.get(var).ok_or(Error::UndeclaredVariable(var.to_owned()))?;
        return Ok(ty.clone());
    }
}
