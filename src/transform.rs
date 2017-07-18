use ast::{CmpOp, ExprCategory, SetOp, TypedExpr};
use types::{Symtable, Type};


pub fn transform(mut expr: TypedExpr, st: &Symtable) -> TypedExpr {
    expr = transform_ors(expr, st);
    return expr;
}

fn transform_ors(expr: TypedExpr, st: &Symtable) -> TypedExpr {
    match expr.expr {
        ExprCategory::Or(subexprs) => {
            if subst_equalities(&subexprs) {
                if let Some(new_expr) = replace_equalities(&subexprs, st, expr.pos, expr.ty.clone()) {
                    return new_expr;
                }
            }
            if subst_ins(&subexprs) {
                if let Some(new_expr) = replace_ins(&subexprs, st, expr.pos, expr.ty.clone()) {
                    return new_expr;
                }
            }
            return TypedExpr {
                    expr: ExprCategory::Or(subexprs),
                    pos: expr.pos,
                    ty: expr.ty
            };
        }
        ExprCategory::And(subexprs) => {
            let subexprs = subexprs.into_iter()
                .map(|e| transform_ors(e, st))
                .collect();
            return TypedExpr {
                    expr: ExprCategory::And(subexprs),
                    pos: expr.pos,
                    ty: expr.ty
            };
        }
        ExprCategory::Not(subexpr) => {
            let subexpr = transform_ors(*subexpr, st);
            return TypedExpr {
                    expr: ExprCategory::Not(Box::new(subexpr)),
                    pos: expr.pos,
                    ty: expr.ty
            };
        }
        _ => { return expr; }
    }
}


// This function looks pretty ugly and complicated for
// what is does:
//
// 1. It checks to make sure that all the expressions in the
//    slice are equalities;
// 2. It gets the variable name used in the first comparison;
// 3. For every comparison, it ensures that the name of the
//    variable name is the same as the one found in step 2.
fn subst_equalities(exprs: &[TypedExpr]) -> bool {
    if exprs.len() < 2 {
        return false;
    }

    for expr in exprs.iter() {
        match expr.expr {
            ExprCategory::Compare(CmpOp::Eq, _, _) => (),
            _ => { return false; }
        }
    }

    let var_name = match get_eq_var_name(&exprs[0]) {
        Some(s) => s,
        None => { return false; }
    };

    for expr in exprs {
        if let ExprCategory::Compare(CmpOp::Eq, ref e1, ref e2) = expr.expr {
            if let ExprCategory::Var(ref s) = e1.expr {
                if *s != *var_name {
                    return false;
                }
            } else if let ExprCategory::Var(ref s) = e2.expr {
                if *s != *var_name {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    return true;
}


fn replace_equalities(exprs: &[TypedExpr], st: &Symtable, pos: usize, ty: Type) -> Option<TypedExpr> {
    let var_name = match get_eq_var_name(&exprs[0]) {
        Some(s) => s,
        None => { return None; }
    };

    let var_ty = match st.get_type(var_name) {
        Some(t) => t,
        None => { return None; }
    };

    let mut consts = Vec::with_capacity(exprs.len());
    for expr in exprs {
        match expr.expr {
            // WTF
            ExprCategory::Compare(CmpOp::Eq, ref e1, ref e2) => {
                match (&e1.expr, &e2.expr) {
                    (&ExprCategory::Var(_), _) => consts.push((**e2).clone()),
                    (_, &ExprCategory::Var(_)) => consts.push((**e1).clone()),
                    (_, _) => { return None; }
                }
            }
            _ => { return None; }
        }
    }

    let var = TypedExpr {
        expr: ExprCategory::Var(var_name.to_string()),
        pos: pos,
        ty: var_ty.clone(),
    };
    let list = TypedExpr {
        expr: ExprCategory::List(consts),
        pos: pos,
        ty: Type::List(Box::new(var_ty))
    };

    return Some(TypedExpr {
        expr: ExprCategory::In(
            Box::new(var),
            Box::new(list)
        ),
        pos: pos,
        ty: ty
    });
}


fn subst_ins(exprs: &[TypedExpr]) -> bool {
    if exprs.len() < 2 {
        return false;
    }

    for expr in exprs.iter() {
        match expr.expr {
            ExprCategory::In(_, _) => (),
            _ => { return false; }
        }
    }

    let var_name = match get_in_var_name(&exprs[0]) {
        Some(s) => s,
        None => { return false; }
    };

    for expr in exprs {
        if let ExprCategory::In(_, ref e2) = expr.expr {
            if let ExprCategory::Var(ref s) = e2.expr {
                if *s != *var_name {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    return true;
}


fn replace_ins(exprs: &[TypedExpr], st: &Symtable, pos: usize, ty: Type) -> Option<TypedExpr> {
    let var_name = match get_in_var_name(&exprs[0]) {
        Some(s) => s,
        None => { return None; }
    };

    let var_ty = match st.get_type(var_name) {
        Some(t) => t,
        None => { return None; }
    };

    let mut consts = Vec::with_capacity(exprs.len());
    for expr in exprs {
        if let ExprCategory::In(ref e1, _) = expr.expr {
            consts.push((**e1).clone());
        } else {
            return None;
        }
    }

    let var = TypedExpr {
        expr: ExprCategory::Var(var_name.to_string()),
        pos: pos,
        ty: var_ty.clone(),
    };
    let list = TypedExpr {
        expr: ExprCategory::List(consts),
        pos: pos,
        ty: Type::List(Box::new(var_ty))
    };

    return Some(TypedExpr {
        expr: ExprCategory::SetOp(
            SetOp::OneOf,
            Box::new(var),
            Box::new(list)
        ),
        pos: pos,
        ty: ty
    });
}


fn get_eq_var_name(expr: &TypedExpr) -> Option<&str> {
    if let ExprCategory::Compare(CmpOp::Eq, ref e1, ref e2) = expr.expr {
        if let ExprCategory::Var(ref s) = e1.expr {
            return Some(s);
        } else if let ExprCategory::Var(ref s) = e2.expr {
            return Some(s);
        } else {
            return None;
        }
    } else {
        return None;
    }
}


fn get_in_var_name(expr: &TypedExpr) -> Option<&str> {
    if let ExprCategory::In(_, ref e2) = expr.expr {
        if let ExprCategory::Var(ref s) = e2.expr {
            return Some(s);
        } else {
            return None;
        }
    } else {
        return None;
    }
}


#[test]
fn test_transform_ors() {
    assert_eq!("n = 1", test::transform_ors(b"n = 1"));
    assert_eq!("s = 'a'", test::transform_ors(b"s = 'a'"));
    assert_eq!("n in (1, 2)", test::transform_ors(b"n = 1 or n = 2"));
    assert_eq!("s in ('a', 'b')", test::transform_ors(b"s = 'a' or s = 'b'"));
    assert_eq!("n in (1, 2)", test::transform_ors(b"1 = n or n = 2"));
    assert_eq!("n in (1, 2)", test::transform_ors(b"n = 1 or 2 = n"));
    assert_eq!("not n in (1, 2)", test::transform_ors(b"not (n = 1 or 2 = n)"));
    assert_eq!("(n in (1, 2) and s in ('a', 'b'))",
               test::transform_ors(b"(n = 1 or n = 2) and (s = 'a' or s = 'b')"));

    assert_eq!("1 in il", test::transform_ors(b"1 in il"));
    assert_eq!("'a' in sl", test::transform_ors(b"'a' in sl"));
    assert_eq!("il one of (1, 2)",
               test::transform_ors(b"1 in il or 2 in il"));
    assert_eq!("sl one of ('a', 'b')",
               test::transform_ors(b"'a' in sl or 'b' in sl"));
    assert_eq!("not il one of (1, 2)",
               test::transform_ors(b"not (1 in il or 2 in il)"));
    assert_eq!("(sl one of ('a', 'b') and il one of (1, 2))",
               test::transform_ors(b"('a' in sl or 'b' in sl) and (1 in il or 2 in il)"));
}


#[cfg(test)]
mod test {
    use parser::Parser;
    use scanner::Scanner;
    use typechecker;
    use types::{Nullable, Type, Symtable};

    pub fn transform_ors(input: &[u8]) -> String {
        let mut st = Symtable::new();
        st.add("n", Type::Int, Nullable::No);
        st.add("s", Type::Str, Nullable::No);
        st.add("il", Type::List(Box::new(Type::Int)), Nullable::No);
        st.add("sl", Type::List(Box::new(Type::Str)), Nullable::No);

        let toks = Scanner::scan(input.to_vec()).expect("scanning failed");
        let expr = Parser::parse(toks).expect("parsing failed");
        let texpr = typechecker::typecheck(expr, &st).expect("typechecking failed");
        let texpr = super::transform_ors(texpr, &st);
        return format!("{}", texpr);
    }
}
