use std::collections::HashMap;

use errors::{Result, Error};
use types::{Nullable, Symtable, Type};

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    ListInt(Vec<i64>),
    ListStr(Vec<String>),
}


/// A run-time environment.
///
/// This data structure associates names (variable names
/// typically) to values.
#[derive(Debug)]
pub struct Env {
    values: HashMap<String, Value>
}


impl Env {
    /// Creates a new, empty environment.
    pub fn new() -> Env {
        Env { values: HashMap::new() }
    }


    /// Adds a new key/value pair to the environment.
    ///
    /// An error is returned when:
    /// - Attempting to add a key/value pair when the key does not
    ///   exist in the symbol table;
    /// - Attempting to give the null value to a key that isn't
    ///   declared nullable in the symbol table;
    /// - Attempting to assign a value incompatible with the key's
    ///   declared type in the symbol table.
    pub fn add<S>(&mut self, key: S, value: Value, st: &Symtable) -> Result<()>
        where S: Into<String>
    {
        let key = key.into();
        let nullable = st.get_nullable(&key).ok_or(Error::UndeclaredVariable(0, key.clone()))?;
        let ty = st.get_type(&key).ok_or(Error::UndeclaredVariable(0, key.clone()))?;
        match value {
            Value::Null => {
                if nullable == Nullable::No {
                    return Err(Error::CannotBeNull(key));
                }
            }
            Value::Bool(_) => {
                if ty != Type::Bool {
                    return Err(Error::EnvInvalidType(key, Type::Bool, ty));
                }
            }
            Value::Int(_) => {
                if ty != Type::Int {
                    return Err(Error::EnvInvalidType(key, Type::Int, ty));
                }
            }
            Value::Float(_) => {
                if ty != Type::Float {
                    return Err(Error::EnvInvalidType(key, Type::Float, ty));
                }
            }
            Value::Str(_) => {
                if ty != Type::Str {
                    return Err(Error::EnvInvalidType(key, Type::Str, ty));
                }
            }
            Value::ListInt(_) => {
                if !ty.is_list_of(Type::Int) {
                    return Err(Error::EnvInvalidType(key, Type::List(Box::new(Type::Int)), ty));
                }
            }
            Value::ListStr(_) => {
                if !ty.is_list_of(Type::Str) {
                    return Err(Error::EnvInvalidType(key, Type::List(Box::new(Type::Str)), ty));
                }
            }
        }
        self.values.insert(key, value);
        return Ok(());
    }


    pub fn get<S>(&self, key: S, st: &Symtable) -> Result<Value>
        where S: Into<String>
    {
        let key = key.into();
        let nullable = st.get_nullable(&key).ok_or(Error::UndeclaredVariable(0, key.clone()))?;
        match self.values.get(&key) {
            None => {
                match nullable {
                    Nullable::No => Err(Error::CannotBeNull(key.clone())),
                    Nullable::Yes => Ok(Value::Null)
                }
            }
            Some(value) => Ok(value.clone())
        }
    }
}


#[test]
fn env_add_null() {
    let st = test::make_symtable();
    let mut env = Env::new();

    assert!(env.add("width", Value::Null, &st).is_err());
    assert!(env.add("postal_code", Value::Null, &st).is_ok());
    assert!(env.add("undeclared", Value::Null, &st).is_err());
    assert!(env.add("undeclared", Value::Null, &st).is_err());
}

#[test]
fn env_add() {
    let st = test::make_symtable();
    let mut env = Env::new();

    assert!(env.add("width", Value::Str("error".to_string()), &st).is_err());
    assert!(env.add("width", Value::Int(42), &st).is_ok());

    assert!(env.add("postal_code", Value::Bool(true), &st).is_err());
    assert!(env.add("postal_code", Value::Str("H0H 0H0".to_string()), &st).is_ok());

    assert!(env.add("private", Value::Int(42), &st).is_err());
    assert!(env.add("private", Value::Bool(false), &st).is_ok());

    assert!(env.add("latitude", Value::Int(42), &st).is_err());
    assert!(env.add("latitude", Value::Float(3.1415), &st).is_ok());

    assert!(env.add("deal_ids", Value::Bool(false), &st).is_err());
    assert!(env.add("deal_ids",
                    Value::ListStr(vec!["a".to_string(), "b".to_string()]),
                    &st).is_ok());

    assert!(env.add("segment_ids", Value::Bool(false), &st).is_err());
    assert!(env.add("segment_ids", Value::ListInt(vec![1,2,3]), &st).is_ok());
}


#[test]
fn env_get() {
    let st = test::make_symtable();
    let mut env = Env::new();

    assert!(env.get("undeclared", &st).is_err());
    assert!(env.get("private", &st).is_err());
    assert!(match env.get("postal_code", &st) {
        Ok(Value::Null) => true,
        _ => false
    });

    let _ = env.add("postal_code", Value::Str("H0H 0H0".to_string()), &st);
    assert!(match env.get("postal_code", &st) {
        Ok(Value::Str(_)) => true,
        _ => false
    });
}


#[cfg(test)]
mod test {
    use types::{Nullable, Symtable, Type};

    pub fn make_symtable() -> Symtable {
        let mut st = Symtable::new();
        st.add("private", Type::Bool, Nullable::No);
        st.add("postal_code", Type::Str, Nullable::Yes);
        st.add("width", Type::Int, Nullable::No);
        st.add("latitude", Type::Float, Nullable::Yes);
        st.add("segment_ids", Type::List(Box::new(Type::Int)), Nullable::No);
        st.add("deal_ids", Type::List(Box::new(Type::Str)), Nullable::Yes);
        return st;
    }
}
