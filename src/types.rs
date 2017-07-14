use std::collections::HashMap;

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


pub fn types_eq(t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (&Type::Bool, &Type::Bool) => true,
        (&Type::Int, &Type::Int) => true,
        (&Type::Float, &Type::Float) => true,
        (&Type::Str, &Type::Str) => true,
        (&Type::List(ref x), &Type::List(ref y)) => types_eq(&*x, &*y),
        _ => false,
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
        let ty = self.symbols.get(var).ok_or(Error::UndeclaredVariable)?;
        return Ok(ty.clone());
    }
}
