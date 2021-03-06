use ast::{CmpOp, SetOp, UntypedExpr, TypedExpr, ExprCategory};
use errors::{Result, Error};
use types::{Type, Symtable};


/// Typechecks an untyped expression given a symbol table.
/// A typed expression is returned (`UntypedExpr` and
/// `TypedExpr` have the same internal representation; they
/// are parametrizer by a different type to represent a
/// Boolean type).
pub fn typecheck(expr: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    let texpr = tc_expr(expr, st)?;
    if texpr.ty != Type::Bool {
        return Err(Error::ExpressionMustBeBool(0));
    }
    return Ok(texpr);
}


fn tc_expr(expr: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    match expr.category {
        ExprCategory::Var(v) => {
            let ty = match st.get_type(&v) {
                Some(t) => t,
                None => {
                    return Err(Error::UndeclaredVariable(expr.pos, v))
                }
            };
            Ok(TypedExpr {
                category: ExprCategory::Var(v),
                pos: expr.pos,
                ty: ty
            })
        }
        ExprCategory::Int(x) =>
            Ok(TypedExpr {
                category: ExprCategory::Int(x),
                pos: expr.pos,
                ty: Type::Int
            }),
        ExprCategory::Float(x) =>
            Ok(TypedExpr {
                category: ExprCategory::Float(x),
                pos: expr.pos,
                ty: Type::Float
            }),
        ExprCategory::Str(s) =>
            Ok(TypedExpr {
                category: ExprCategory::Str(s),
                pos: expr.pos,
                ty: Type::Str
            }),
        ExprCategory::List(elems) =>
            tc_list(elems, expr.pos, st),
        ExprCategory::Compare(op, e1, e2) =>
            tc_compare(op, *e1, *e2, st),
        ExprCategory::Call(func_name, args) =>
            tc_call(func_name, args, expr.pos, st),
        ExprCategory::In(needle, haystack) =>
            tc_in(*needle, *haystack, st),
        ExprCategory::SetOp(op, x, y) =>
            tc_set_op(op, *x, *y, st),
        ExprCategory::IsNull(var) =>
            tc_is_null(var, expr.pos, st),
        ExprCategory::And(exprs) => {
            let texprs = tc_bool_vec(exprs, st)?;
            Ok(TypedExpr {
                category: ExprCategory::And(texprs),
                pos: expr.pos,
                ty: Type::Bool

            })
        }
        ExprCategory::Or(exprs) => {
            let texprs = tc_bool_vec(exprs, st)?;
            Ok(TypedExpr {
                category: ExprCategory::Or(texprs),
                pos: expr.pos,
                ty: Type::Bool
            })
        }
        ExprCategory::Not(expr) => {
            let texpr = tc_expr(*expr, st)?;
            let is_ok = match &texpr.ty {
                &Type::Bool => true,
                _ => false
            };
            if !is_ok {
                return Err(Error::IncorrectType(texpr.pos, Type::Bool, texpr.ty));
            }
            let pos = texpr.pos;
            return Ok(TypedExpr {
                category: ExprCategory::Not(Box::new(texpr)),
                pos: pos,
                ty: Type::Bool
            });
        }
    }
}


fn tc_list(exprs: Vec<UntypedExpr>, pos: usize, st: &Symtable) -> Result<TypedExpr> {
    if exprs.is_empty() {
        return Err(Error::EmptyList(pos));
    }

    let mut texprs = Vec::with_capacity(exprs.len());
    for expr in exprs {
        texprs.push(tc_expr(expr, st)?);
    }

    let ty = texprs[0].ty.clone();
    if ty != Type::Int && ty != Type::Str {
        return Err(Error::InvalidListType(pos, ty));
    }

    for texpr in texprs.iter() {
        if texpr.ty != ty {
            return Err(Error::IncorrectType(pos, ty, texpr.ty.clone()));
        }
    }

    return Ok(TypedExpr {
        category: ExprCategory::List(texprs),
        pos: pos,
        ty: Type::List(Box::new(ty))
    });
}


fn tc_compare(op: CmpOp, e1: UntypedExpr, e2: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    enum CmpValidity { Ok, DisallowedOperation, TypeMismatch };

    let te1 = tc_expr(e1, st)?;
    let te2 = tc_expr(e2, st)?;

    let validity =
        if te1.ty == Type::Int || te1.ty == Type::Float {
            if te1.ty == te2.ty {
                CmpValidity::Ok
            } else {
                CmpValidity::TypeMismatch
            }
        } else if te1.ty == Type::Str {
            if op == CmpOp::Eq || op == CmpOp::Ne {
                if te2.ty == Type::Str {
                    CmpValidity::Ok
                } else {
                    CmpValidity::TypeMismatch
                }
            } else {
                CmpValidity::DisallowedOperation
            }
        } else {
            CmpValidity::DisallowedOperation
        };

    let pos = te1.pos;
    match validity {
        CmpValidity::Ok => {
            return Ok(TypedExpr {
                category: ExprCategory::Compare(op, Box::new(te1), Box::new(te2)),
                pos: pos,
                ty: Type::Bool
            });
        }
        CmpValidity::DisallowedOperation => {
            return Err(Error::InvalidOperation(pos));
        }
        CmpValidity::TypeMismatch => {
            return Err(Error::IncorrectType(te2.pos, te1.ty, te2.ty));
        }
    }
}


fn tc_in(needle: UntypedExpr, haystack: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    let te1 = tc_expr(needle, st)?;
    let te2 = tc_expr(haystack, st)?;

    let is_ok = match (&te1.ty, &te2.ty) {
        (t, &Type::List(ref x)) =>
            (*t == Type::Int || *t == Type::Str) && *t == **x,
        _ => false
    };

    let pos = te1.pos;
    if !is_ok {
        return Err(Error::InvalidSetOperation(pos));
    }
    return Ok(TypedExpr {
        category: ExprCategory::In(Box::new(te1), Box::new(te2)),
        pos: pos,
        ty: Type::Bool
    });
}


fn tc_set_op(op: SetOp, e1: UntypedExpr, e2: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    let te1 = tc_expr(e1, st)?;
    let te2 = tc_expr(e2, st)?;

    let is_ok = match (&te1.ty, &te2.ty) {
        (&Type::List(ref x), &Type::List(ref y)) =>
            (**x == Type::Int && **y == Type::Int) ||
            (**x == Type::Str && **y == Type::Str),
        _ => false
    };

    let pos = te1.pos;
    if !is_ok {
        return Err(Error::InvalidSetOperation(pos));
    }
    return Ok(TypedExpr {
        category: ExprCategory::SetOp(op, Box::new(te1), Box::new(te2)),
        pos: pos,
        ty: Type::Bool
    });
}


fn tc_is_null(var: String, pos: usize, st: &Symtable) -> Result<TypedExpr> {
    let _ = st.get_type(&var)
        .ok_or(Error::UndeclaredVariable(pos, var.clone()))?;
    return Ok(TypedExpr {
        category: ExprCategory::IsNull(var),
        pos: pos,
        ty: Type::Bool,
    });
}


fn tc_bool_vec(exprs: Vec<UntypedExpr>, st: &Symtable) -> Result<Vec<TypedExpr>> {
    let mut texprs = Vec::with_capacity(exprs.len());
    for expr in exprs {
        let texpr = tc_expr(expr, st)?;
        match &texpr.ty {
            &Type::Bool => texprs.push(texpr),
            _ => { return Err(Error::IncorrectType(texpr.pos, Type::Bool, texpr.ty)) }
        }
    }
    return Ok(texprs);
}


fn tc_call(func_name: String, args: Vec<UntypedExpr>, pos: usize, st: &Symtable) -> Result<TypedExpr> {
    let mut targs = Vec::with_capacity(args.len());

    let func_type = st.get_type(&func_name)
        .ok_or(Error::UndeclaredVariable(pos, func_name.clone()))?;
    let (expected, result) = match func_type {
        Type::Func(args, res) => (args, res),
        _ => { return Err(Error::NotAFunction(pos, func_name)) }
    };

    if args.len() != expected.len() {
        return Err(Error::IncorrectArgListLength(pos, expected.len(), args.len()));
    }

    for (arg, ty) in args.into_iter().zip(expected) {
        let targ = tc_expr(arg, st)?;
        if targ.ty != ty {
            return Err(Error::IncorrectType(pos, ty, targ.ty));
        }
        targs.push(targ);
    }
    return Ok(TypedExpr {
        category: ExprCategory::Call(func_name, targs),
        pos: pos,
        ty: *result
    });
}


#[test]
fn test_compare() {
    assert!(test::tc(b"i = 1").is_ok());
    assert!(test::tc(b"i <> 1").is_ok());
    assert!(test::tc(b"i < 1").is_ok());
    assert!(test::tc(b"i <= 1").is_ok());
    assert!(test::tc(b"i > 1").is_ok());
    assert!(test::tc(b"i >= 1").is_ok());

    assert!(test::tc(b"f = 1.0").is_ok());
    assert!(test::tc(b"f <> 1.0").is_ok());
    assert!(test::tc(b"f < 1.0").is_ok());
    assert!(test::tc(b"f <= 1.0").is_ok());
    assert!(test::tc(b"f > 1.0").is_ok());
    assert!(test::tc(b"f >= 1.0").is_ok());

    assert!(test::tc(b"s = 'abc'").is_ok());
    assert!(test::tc(b"s <> 'abc'").is_ok());
    assert!(test::tc(b"s < 'abc'").is_err());
    assert!(test::tc(b"s <= 'abc'").is_err());
    assert!(test::tc(b"s > 'abc'").is_err());
    assert!(test::tc(b"s >= 'abc'").is_err());

    assert!(test::tc(b"il = (1,2)").is_err());
    assert!(test::tc(b"il <> (1,2)").is_err());
    assert!(test::tc(b"il < (1,2)").is_err());
    assert!(test::tc(b"il <= (1,2)").is_err());
    assert!(test::tc(b"il > (1,2)").is_err());
    assert!(test::tc(b"il >= (1,2)").is_err());

    assert!(test::tc(b"sl = ('abc', 'def')").is_err());
    assert!(test::tc(b"sl <> ('abc', 'def')").is_err());
    assert!(test::tc(b"sl < ('abc', 'def')").is_err());
    assert!(test::tc(b"sl <= ('abc', 'def')").is_err());
    assert!(test::tc(b"sl > ('abc', 'def')").is_err());
    assert!(test::tc(b"sl >= ('abc', 'def')").is_err());

    assert!(test::tc(b"b = b").is_err());
    assert!(test::tc(b"b <> b").is_err());
    assert!(test::tc(b"b < b").is_err());
    assert!(test::tc(b"b <= b").is_err());
    assert!(test::tc(b"b > b").is_err());
    assert!(test::tc(b"b >= b").is_err());

    assert!(match test::tc(b"3 = 'foo'") {
        Err(Error::IncorrectType(_, _, _)) => true,
        _ => false
    });
    assert!(match test::tc(b"'foo' = 3") {
        Err(Error::IncorrectType(_, _, _)) => true,
        _ => false
    });
    assert!(match test::tc(b"'foo' < 3") {
        Err(Error::InvalidOperation(_)) => true,
        _ => false
    });
}


#[test]
fn test_set_ops() {
    assert!(test::tc(b"i in (1, 2, 3)").is_ok());
    assert!(test::tc(b"b in (1, 2, 3)").is_err());
    assert!(test::tc(b"f in (1, 2, 3)").is_err());
    assert!(test::tc(b"s in (1, 2, 3)").is_err());
    assert!(test::tc(b"il in (1, 2, 3)").is_err());
    assert!(test::tc(b"sl in (1, 2, 3)").is_err());

    assert!(test::tc(b"s in ('abc', 'def')").is_ok());
    assert!(test::tc(b"b in ('abc', 'def')").is_err());
    assert!(test::tc(b"i in ('abc', 'def')").is_err());
    assert!(test::tc(b"f in ('abc', 'def')").is_err());
    assert!(test::tc(b"il in ('abc', 'def')").is_err());
    assert!(test::tc(b"sl in ('abc', 'def')").is_err());

    assert!(test::tc(b"sl none of ('abc', 'def')").is_ok());
    assert!(test::tc(b"sl one of  ('abc', 'def')").is_ok());
    assert!(test::tc(b"sl all of  ('abc', 'def')").is_ok());
    assert!(test::tc(b"il none of ('abc', 'def')").is_err());
    assert!(test::tc(b"il one of  ('abc', 'def')").is_err());
    assert!(test::tc(b"il all of  ('abc', 'def')").is_err());
    assert!(test::tc(b"i none of ('abc', 'def')").is_err());
    assert!(test::tc(b"i one of  ('abc', 'def')").is_err());
    assert!(test::tc(b"i all of  ('abc', 'def')").is_err());
    assert!(test::tc(b"b none of ('abc', 'def')").is_err());
    assert!(test::tc(b"b one of  ('abc', 'def')").is_err());
    assert!(test::tc(b"b all of  ('abc', 'def')").is_err());
    assert!(test::tc(b"f none of ('abc', 'def')").is_err());
    assert!(test::tc(b"f one of  ('abc', 'def')").is_err());
    assert!(test::tc(b"f all of  ('abc', 'def')").is_err());
    assert!(test::tc(b"s none of ('abc', 'def')").is_err());
    assert!(test::tc(b"s one of  ('abc', 'def')").is_err());
    assert!(test::tc(b"s all of  ('abc', 'def')").is_err());

    assert!(test::tc(b"il none of (1, 2, 3)").is_ok());
    assert!(test::tc(b"il one of  (1, 2, 3)").is_ok());
    assert!(test::tc(b"il all of  (1, 2, 3)").is_ok());
    assert!(test::tc(b"sl none of (1, 2, 3)").is_err());
    assert!(test::tc(b"sl one of  (1, 2, 3)").is_err());
    assert!(test::tc(b"sl all of  (1, 2, 3)").is_err());
    assert!(test::tc(b"i none of (1, 2, 3)").is_err());
    assert!(test::tc(b"i one of  (1, 2, 3)").is_err());
    assert!(test::tc(b"i all of  (1, 2, 3)").is_err());
    assert!(test::tc(b"b none of (1, 2, 3)").is_err());
    assert!(test::tc(b"b one of  (1, 2, 3)").is_err());
    assert!(test::tc(b"b all of  (1, 2, 3)").is_err());
    assert!(test::tc(b"f none of (1, 2, 3)").is_err());
    assert!(test::tc(b"f one of  (1, 2, 3)").is_err());
    assert!(test::tc(b"f all of  (1, 2, 3)").is_err());
    assert!(test::tc(b"s none of (1, 2, 3)").is_err());
    assert!(test::tc(b"s one of  (1, 2, 3)").is_err());
    assert!(test::tc(b"s all of  (1, 2, 3)").is_err());
}

#[test]
fn test_call() {
    assert!(test::tc(b"has_len('foo', 3)").is_ok());
    assert!(test::tc(b"has_len('foo')").is_err());
    assert!(test::tc(b"has_len('foo', 3, 4)").is_err());
    assert!(test::tc(b"has_len(3, 'foo')").is_err());
    assert!(test::tc(b"il(3)").is_err());
}

#[test]
fn test_expr_is_bool() {
    assert!(test::tc(b"b").is_ok());
    assert!(test::tc(b"has_len('foo', 3)").is_ok());
    assert!(test::tc(b"i").is_err());
    assert!(test::tc(b"f").is_err());
    assert!(test::tc(b"s").is_err());
    assert!(test::tc(b"il").is_err());
    assert!(test::tc(b"sl").is_err());
}


#[cfg(test)]
mod test {
    use super::typecheck;
    use ast::TypedExpr;
    use errors::Result;
    use parser::Parser;
    use scanner::Scanner;
    use types::{Nullable, Symtable, Type};

    pub fn tc(input: &[u8]) -> Result<TypedExpr> {
        let toks = Scanner::scan(input.to_vec())?;
        let uexpr = Parser::parse(toks)?;
        let mut st = Symtable::new();
        st.add("b", Type::Bool, Nullable::No);
        st.add("i", Type::Int, Nullable::No);
        st.add("f", Type::Float, Nullable::No);
        st.add("s", Type::Str, Nullable::No);
        st.add("il", Type::List(Box::new(Type::Int)), Nullable::No);
        st.add("sl", Type::List(Box::new(Type::Str)), Nullable::No);
        st.add("has_len",
               Type::Func(vec![Type::Str, Type::Int], Box::new(Type::Bool)), Nullable::No);
        return typecheck(uexpr, &st);
    }
}
