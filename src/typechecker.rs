use ast::{CmpOp, SetOp, UntypedExpr, TypedExpr, ExprCategory};
use errors::{Result, Error};
use types::{Type, Symtable};


pub fn tc_expr(expr: UntypedExpr, st: &Symtable) -> Result<TypedExpr> {
    match expr.expr {
        ExprCategory::Var(v) => {
            let ty = st.get(&v)?;
            Ok(TypedExpr {
                expr: ExprCategory::Var(v),
                pos: expr.pos,
                ty: ty
            })
        }
        ExprCategory::Int(x) =>
            Ok(TypedExpr {
                expr: ExprCategory::Int(x),
                pos: expr.pos,
                ty: Type::Int
            }),
        ExprCategory::Float(x) =>
            Ok(TypedExpr {
                expr: ExprCategory::Float(x),
                pos: expr.pos,
                ty: Type::Float
            }),
        ExprCategory::Str(s) =>
            Ok(TypedExpr {
                expr: ExprCategory::Str(s),
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
                expr: ExprCategory::And(texprs),
                pos: expr.pos,
                ty: Type::Bool

            })
        }
        ExprCategory::Or(exprs) => {
            let texprs = tc_bool_vec(exprs, st)?;
            Ok(TypedExpr {
                expr: ExprCategory::Or(texprs),
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
                expr: ExprCategory::Not(Box::new(texpr)),
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
        expr: ExprCategory::List(texprs),
        pos: pos,
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

    let pos = te1.pos;
    if !is_ok {
        return Err(Error::IncorrectType(te2.pos, te1.ty, te2.ty));
    }
    return Ok(TypedExpr {
        expr: ExprCategory::Compare(op, Box::new(te1), Box::new(te2)),
        pos: pos,
        ty: Type::Bool
    });
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
        expr: ExprCategory::In(Box::new(te1), Box::new(te2)),
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
        expr: ExprCategory::SetOp(op, Box::new(te1), Box::new(te2)),
        pos: pos,
        ty: Type::Bool
    });
}


fn tc_is_null(var: String, pos: usize, st: &Symtable) -> Result<TypedExpr> {
    let _ = st.get(&var)?;
    return Ok(TypedExpr {
        expr: ExprCategory::IsNull(var),
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

    let func_type = st.get(&func_name)?;
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
        expr: ExprCategory::Call(func_name, targs),
        pos: pos,
        ty: *result
    });
}
