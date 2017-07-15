use std::collections::HashMap;
use std::fmt;
use std::convert::Into;

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


    pub fn add<T>(&mut self, var: T, ty: Type)
        where T: Into<String>
    {
        self.symbols.insert(var.into(), ty);
    }


    pub fn get(&self, var: &str) -> Option<Type> {
        return self.symbols.get(var).map(|ty| ty.clone());
    }
}
