use std::collections::HashMap;
use std::fmt;
use std::convert::Into;


/// The types of the Boolean language.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Int,
    Float,
    Str,
    List(Box<Type>),
    Func(Vec<Type>, Box<Type>),
}

impl Type {
    pub fn is_list_of(&self, expected_ty: Type) -> bool {
        match *self {
            Type::List(ref actual_ty) => **actual_ty == expected_ty,
            _ => false
        }
    }
}



/// An enum to specify whether a variable can
/// be missing from the evaluation environment.
/// Isomorphic to a `bool`, but more descriptive.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Nullable {
    No,
    Yes,
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


/// A symbol table that associates a variable
/// with a type and whether this variable can
/// be absent from the evaluation environment.
pub struct Symtable {
    symbols: HashMap<String, (Type, Nullable)>
}


impl Symtable {
    /// Creates a new, empty symbol table.
    pub fn new() -> Symtable {
        return Symtable { symbols: HashMap::with_capacity(64) };
    }


    /// Inserts a new binding to the symbol table.
    pub fn add<T>(&mut self, var: T, ty: Type, n: Nullable)
        where T: Into<String>
    {
        self.symbols.insert(var.into(), (ty, n));
    }


    /// Returns the the type of a variable, or `None`
    /// if the variable isn't in the table.
    pub fn get_type(&self, var: &str) -> Option<Type> {
        return self.symbols.get(var).map(|&(ref ty, _)| ty.clone());
    }


    /// Returns the the nullability of a variable, or `None`
    /// if the variable isn't in the table.
    pub fn get_nullable(&self, var: &str) -> Option<Nullable> {
        return self.symbols.get(var).map(|&(_, n)| n);
    }
}
