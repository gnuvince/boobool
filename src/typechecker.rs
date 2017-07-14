use ast::{CmpOp, SetOp, UntypedExpr, TypedExpr, ExprCategory};
use errors::{Result, Error};
use types::{Type, Symtable, types_eq};


pub fn tc_expr(expr: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    match expr.expr {
        ExprCategory::Var(v) => {
            let ty = st.get(&v)?;
            Ok(TypedExpr {
                expr: ExprCategory::Var(v),
                ty: ty
            })
        }
        ExprCategory::Int(x) =>
            Ok(TypedExpr {
                expr: ExprCategory::Int(x),
                ty: Type::Int
            }),
        ExprCategory::Float(x) =>
            Ok(TypedExpr {
                expr: ExprCategory::Float(x),
                ty: Type::Float
            }),
        ExprCategory::Str(s) =>
            Ok(TypedExpr {
                expr: ExprCategory::Str(s),
                ty: Type::Str
            }),
        ExprCategory::List(elems) =>
            tc_list(elems, st),
        ExprCategory::Compare(op, e1, e2) =>
            tc_compare(op, *e1, *e2, st),
        ExprCategory::In(needle, haystack) =>
            tc_in(*needle, *haystack, st),
        ExprCategory::SetOp(op, x, y) =>
            tc_set_op(op, *x, *y, st),
        ExprCategory::IsNull(var) =>
            tc_is_null(var, st),
        ExprCategory::And(exprs) => {
            let texprs = tc_bool_vec(exprs, st)?;
            Ok(TypedExpr {
                expr: ExprCategory::And(texprs),
                ty: Type::Bool

            })
        }
        ExprCategory::Or(exprs) => {
            let texprs = tc_bool_vec(exprs, st)?;
            Ok(TypedExpr {
                expr: ExprCategory::Or(texprs),
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
                return Err(Error::IncorrectType);
            }
            return Ok(TypedExpr {
                expr: ExprCategory::Not(Box::new(texpr)),
                ty: Type::Bool
            });
        }

        _ => unimplemented!()
    }
}


fn tc_list(exprs: Vec<UntypedExpr>, st: &Symtable) -> Result<TypedExpr> {
    if exprs.is_empty() {
        return Err(Error::EmptyList);
    }

    let mut texprs = Vec::with_capacity(exprs.len());
    for expr in exprs {
        texprs.push(tc_expr(expr, st)?);
    }

    let ty = texprs[0].ty.clone();
    if ty != Type::Int && ty != Type::Str {
        return Err(Error::InvalidListType);
    }

    for texpr in texprs.iter() {
        if texpr.ty != ty {
            return Err(Error::IncorrectType);
        }
    }

    return Ok(TypedExpr {
        expr: ExprCategory::List(texprs),
        ty: Type::List(Box::new(ty))
    });
}


fn tc_compare(op: CmpOp, e1: UntypedExpr, e2: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    let te1 = tc_expr(e1, st)?;
    let te2 = tc_expr(e2, st)?;

    let is_ok = match (&te1.ty, &te2.ty) {
        (&Type::Int, &Type::Int) => true,
        (&Type::Float, &Type::Float) => true,
        (&Type::Str, &Type::Str) =>
            op == CmpOp::Eq || op == CmpOp::Ne,
        _ => false
    };

    if !is_ok {
        return Err(Error::InvalidComparison);
    }

    return Ok(TypedExpr {
        expr: ExprCategory::Compare(op, Box::new(te1), Box::new(te2)),
        ty: Type::Bool
    });
}


fn tc_in(needle: UntypedExpr, haystack: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    let te1 = tc_expr(needle, st)?;
    let te2 = tc_expr(haystack, st)?;

    let is_ok = match (&te1.ty, &te2.ty) {
        (t, &Type::List(ref x)) =>
            (types_eq(t, &Type::Int) || types_eq(t, &Type::Str)) && types_eq(t, x),
        _ => false
    };

    if !is_ok {
        return Err(Error::InvalidSetOperation);
    }

    return Ok(TypedExpr {
        expr: ExprCategory::In(Box::new(te1), Box::new(te2)),
        ty: Type::Bool
    });
}


fn tc_set_op(op: SetOp, e1: UntypedExpr, e2: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    let te1 = tc_expr(e1, st)?;
    let te2 = tc_expr(e2, st)?;

    let is_ok = match (&te1.ty, &te2.ty) {
        (&Type::List(ref x), &Type::List(ref y)) =>
            (types_eq(x, &Type::Int) && types_eq(y, &Type::Int)) ||
            (types_eq(x, &Type::Str) && types_eq(y, &Type::Str)),
        x => {println!("{:?}", x);false}
    };

    if !is_ok {
        return Err(Error::InvalidSetOperation);
    }

    return Ok(TypedExpr {
        expr: ExprCategory::SetOp(op, Box::new(te1), Box::new(te2)),
        ty: Type::Bool
    });
}


fn tc_is_null(var: String, st: &Symtable) -> Result<TypedExpr> {
    let _ = st.get(&var)?;
    return Ok(TypedExpr {
        expr: ExprCategory::IsNull(var),
        ty: Type::Bool,
    });
}


fn tc_bool_vec(exprs: Vec<UntypedExpr>, st: &Symtable) -> Result<Vec<TypedExpr>> {
    let mut texprs = Vec::with_capacity(exprs.len());
    for expr in exprs {
        let texpr = tc_expr(expr, st)?;
        match &texpr.ty {
            &Type::Bool => texprs.push(texpr),
            _ => { return Err(Error::IncorrectType) }
        }
    }
    return Ok(texprs);
}
