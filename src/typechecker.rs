use ast::{CmpOp, Expr, TExpr, SetOp};
use ast::TExprEnum as T;
use errors::{Result, Error};
use types::{Type, Symtable, types_eq};


pub fn tc_expr(expr: Expr, st: &Symtable) -> Result<TExpr> {
    match expr {
        Expr::Var(v) => {
            let ty = st.get(&v)?;
            Ok(TExpr { expr: T::Var(v), ty: ty })
        }
        Expr::Int(x) =>
            Ok(TExpr { expr: T::Int(x), ty: Type::Int }),
        Expr::Float(x) =>
            Ok(TExpr { expr: T::Float(x), ty: Type::Float }),
        Expr::Str(s) =>
            Ok(TExpr { expr: T::Str(s), ty: Type::Str }),
        Expr::List(elems) =>
            tc_list(elems, st),
        Expr::Compare(op, e1, e2) =>
            tc_compare(op, *e1, *e2, st),
        Expr::In(needle, haystack) =>
            tc_in(*needle, *haystack, st),
        Expr::SetOp(op, x, y) =>
            tc_set_op(op, *x, *y, st),
        Expr::IsNull(var) =>
            tc_is_null(var, st),
        Expr::And(exprs) => {
            let texprs = tc_bool_vec(exprs, st)?;
            Ok(TExpr {
                expr: T::And(texprs),
                ty: Type::Bool
            })
        }
        Expr::Or(exprs) => {
            let texprs = tc_bool_vec(exprs, st)?;
            Ok(TExpr {
                expr: T::Or(texprs),
                ty: Type::Bool
            })
        }
        Expr::Not(expr) => {
            let texpr = tc_expr(*expr, st)?;
            let is_ok = match &texpr.ty {
                &Type::Bool => true,
                _ => false
            };
            if !is_ok {
                return Err(Error::IncorrectType);
            }
            return Ok(TExpr {
                expr: T::Not(Box::new(texpr)),
                ty: Type::Bool
            });
        }

        _ => unimplemented!()
    }
}


fn tc_list(exprs: Vec<Expr>, st: &Symtable) -> Result<TExpr> {
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

    return Ok(TExpr {
        expr: T::List(texprs),
        ty: ty
    });
}


fn tc_compare(op: CmpOp, e1: Expr, e2: Expr, st: &Symtable) -> Result<TExpr> {
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

    return Ok(TExpr {
        expr: T::Compare(op, Box::new(te1), Box::new(te2)),
        ty: Type::Bool
    });
}


fn tc_in(needle: Expr, haystack: Expr, st: &Symtable) -> Result<TExpr> {
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

    return Ok(TExpr {
        expr: T::In(Box::new(te1), Box::new(te2)),
        ty: Type::Bool
    });
}


fn tc_set_op(op: SetOp, e1: Expr, e2: Expr, st: &Symtable) -> Result<TExpr> {
    let te1 = tc_expr(e1, st)?;
    let te2 = tc_expr(e2, st)?;

    let is_ok = match (&te1.ty, &te2.ty) {
        (&Type::List(ref x), &Type::List(ref y)) =>
            (types_eq(x, &Type::Int) || types_eq(x, &Type::Str)) && types_eq(x, y),
        _ => false
    };

    if !is_ok {
        return Err(Error::InvalidSetOperation);
    }

    return Ok(TExpr {
        expr: T::SetOp(op, Box::new(te1), Box::new(te2)),
        ty: Type::Bool
    });
}


fn tc_is_null(var: String, st: &Symtable) -> Result<TExpr> {
    let _ = st.get(&var)?;
    return Ok(TExpr {
        expr: T::IsNull(var),
        ty: Type::Bool,
    });
}


fn tc_bool_vec(exprs: Vec<Expr>, st: &Symtable) -> Result<Vec<TExpr>> {
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
