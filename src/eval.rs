use std::collections::HashMap;

use ast::{CmpOp, SetOp, ExprCategory, TypedExpr};
use errors::{Result, Error};
use types::{Nullable, Symtable, Type};


pub fn eval(expr: TypedExpr, st: &Symtable, env: &Env) -> Result<bool> {
    let return_val = value_expr(expr, st, env)?;
    match return_val {
        Value::Bool(b) => Ok(b),
        _ => Err(Error::EvalError)
    }
}


fn value_expr(expr: TypedExpr, st: &Symtable, env: &Env) -> Result<Value> {
    match expr.category {
        ExprCategory::Var(s) => env.get(s, st),
        ExprCategory::Int(n) => Ok(Value::Int(n)),
        ExprCategory::Float(f) => Ok(Value::Float(f)),
        ExprCategory::Str(s) => Ok(Value::Str(s)),
        ExprCategory::List(xs) => {
            if expr.ty.is_list_of(Type::Int) {
                return int_list(xs, st, env);
            } else if expr.ty.is_list_of(Type::Str) {
                return str_list(xs, st, env);
            } else {
                return Err(Error::EvalError);
            }
        }
        ExprCategory::Compare(op, a, b) => value_compare(op, *a, *b, st, env),
        ExprCategory::And(subexprs) => {
            for subexpr in subexprs {
                match value_expr(subexpr, st, env)? {
                    Value::Bool(b) => {
                        if !b {
                            return Ok(Value::Bool(false));
                        }
                    }
                    _ => { return Err(Error::EvalError); }
                }

            }
            return Ok(Value::Bool(true));
        }
        ExprCategory::Or(subexprs) => {
            for subexpr in subexprs {
                match value_expr(subexpr, st, env)? {
                    Value::Bool(b) => {
                        if b {
                            return Ok(Value::Bool(true));
                        }
                    }
                    _ => { return Err(Error::EvalError); }
                }

            }
            return Ok(Value::Bool(false));
        }
        ExprCategory::Not(subexpr) => {
            let val = value_expr(*subexpr, st, env)?;
            match val {
                Value::Bool(b) => { return Ok(Value::Bool(!b)); }
                _ => { return Err(Error::EvalError); }
            }
        }
        ExprCategory::IsNull(var) => {
            match env.get(var, st) {
                Ok(Value::Null) => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false))
            }
        }
        ExprCategory::SetOp(op, left, right) =>
            value_setop(op, *left, *right, st, env),
        ExprCategory::In(needle, haystack) => {
            let needle_val = value_expr(*needle, st, env)?;
            let haystack_val = value_expr(*haystack, st, env)?;
            match (needle_val, haystack_val) {
                (Value::Int(x), Value::ListInt(xs)) =>
                    Ok(Value::Bool(xs.binary_search(&x).is_ok())),
                (Value::Str(x), Value::ListStr(xs)) =>
                    Ok(Value::Bool(xs.binary_search(&x).is_ok())),
                (_, _) =>
                    Err(Error::EvalError)
            }
        }
        _ => unimplemented!()
    }
}


fn value_compare(op: CmpOp, left: TypedExpr, right: TypedExpr,
                 st: &Symtable, env: &Env) -> Result<Value> {
    let left_val = value_expr(left, st, env)?;
    let right_val = value_expr(right, st, env)?;
    match (left_val, right_val) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(cmp_op(op, x, y))),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Bool(cmp_op(op, x, y))),
        (Value::Str(x), Value::Str(y)) => Ok(Value::Bool(cmp_op(op, x, y))),
        _ => Err(Error::InvalidOperation(0))
    }
}


fn value_setop(op: SetOp, left: TypedExpr, right: TypedExpr,
               st: &Symtable, env: &Env) -> Result<Value> {
    let left_val = value_expr(left, st, env)?;
    let right_val = value_expr(right, st, env)?;
    match (left_val, right_val) {
        (Value::ListInt(xs), Value::ListInt(ys)) => Ok(Value::Bool(set_op(op, xs, ys))),
        (Value::ListStr(xs), Value::ListStr(ys)) => Ok(Value::Bool(set_op(op, xs, ys))),
        _ => Err(Error::EvalError)
    }
}


fn cmp_op<T: PartialEq + PartialOrd>(op: CmpOp, x: T, y: T) -> bool {
    match op {
        CmpOp::Eq => x == y,
        CmpOp::Ne => x != y,
        CmpOp::Lt => x <  y,
        CmpOp::Le => x <= y,
        CmpOp::Gt => x >  y,
        CmpOp::Ge => x >= y,
    }
}

fn set_op<T: Eq + Ord>(op: SetOp, xs: Vec<T>, ys: Vec<T>) -> bool {
    match op {
        SetOp::NoneOf =>
            !xs.into_iter().any(|x| ys.binary_search(&x).is_ok()),
        SetOp::OneOf =>
            xs.into_iter().any(|x| ys.binary_search(&x).is_ok()),
        SetOp::AllOf =>
            xs.into_iter().all(|x| ys.binary_search(&x).is_ok())
    }
}

fn int_list(xs: Vec<TypedExpr>, st: &Symtable, env: &Env) -> Result<Value> {
    let mut elems = Vec::with_capacity(xs.len());
    for x in xs {
        match value_expr(x, st, env)? {
            Value::Int(n) => { elems.push(n); }
            _ => { return Err(Error::EvalError); }
        }
    }
    return Ok(Value::ListInt(elems));
}

fn str_list(xs: Vec<TypedExpr>, st: &Symtable, env: &Env) -> Result<Value> {
    let mut elems = Vec::with_capacity(xs.len());
    for x in xs {
        match value_expr(x, st, env)? {
            Value::Str(n) => { elems.push(n); }
            _ => { return Err(Error::EvalError); }
        }
    }
    return Ok(Value::ListStr(elems));
}


/// A run-time value
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


    pub fn count(&self) -> usize {
        return self.values.len();
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
    assert_eq!(1, env.count());
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


#[test]
fn test_eval_var() {
    let st = test::make_symtable();
    let mut env = Env::new();
    assert!(test::eval(b"private", &st, &env).is_err());
    assert!(match test::eval(b"postal_code = 'H0H 0H0'", &st, &env) {
        Ok(false) => true,
        _ => false
    });

    let _ = env.add("postal_code", Value::Str("H0H 0H0".to_string()), &st);
    assert!(match test::eval(b"postal_code = 'H0H 0H0'", &st, &env) {
        Ok(true) => true,
        _ => false
    });
}

#[test]
fn test_eval_set() {
    let st = test::make_symtable();
    let mut env = Env::new();
    let _ = env.add("segment_ids",
                    Value::ListInt(vec![2, 3, 5, 7]),
                    &st);
    let _ = env.add("deal_ids",
                    Value::ListStr(vec!["AG1".to_string(), "AG2".to_string()]),
                    &st);

    assert!(match test::eval(b"1 in segment_ids", &st, &env) {
        Ok(false) => true,
        _ => false
    });
    assert!(match test::eval(b"1 not in segment_ids", &st, &env) {
        Ok(true) => true,
        _ => false
    });
    assert!(match test::eval(b"2 in segment_ids", &st, &env) {
        Ok(true) => true,
        _ => false
    });
    assert!(match test::eval(b"2 not in segment_ids", &st, &env) {
        Ok(false) => true,
        _ => false
    });

    assert!(match test::eval(b"segment_ids none of (2, 8)", &st, &env) {
        Ok(false) => true,
        _ => false
    });
    assert!(match test::eval(b"segment_ids none of (8)", &st, &env) {
        Ok(true) => true,
        _ => false
    });
    assert!(match test::eval(b"segment_ids one of (2, 8)", &st, &env) {
        Ok(true) => true,
        _ => false
    });
    assert!(match test::eval(b"segment_ids one of (8)", &st, &env) {
        Ok(false) => true,
        _ => false
    });
    assert!(match test::eval(b"segment_ids all of (2, 3)", &st, &env) {
        Ok(false) => true,
        _ => false
    });
    assert!(match test::eval(b"segment_ids all of (2, 3, 5, 7, 11)", &st, &env) {
        Ok(true) => true,
        _ => false
    });
}


#[cfg(test)]
mod test {
    use eval::{Env};
    use errors::Result;
    use parser::Parser;
    use scanner::Scanner;
    use transformers;
    use typechecker;
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

    pub fn eval(input: &[u8], st: &Symtable, env: &Env) -> Result<bool> {
        let toks = Scanner::scan(input.to_vec())?;
        let expr = Parser::parse(toks)?;
        let texpr = typechecker::typecheck(expr, st)?;
        let texpr = transformers::transform(texpr, st);
        return super::eval(texpr, st, env);
    }
}
