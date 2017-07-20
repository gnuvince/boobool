use ast::{ExprCategory, TypedExpr};
use types::Type;


pub fn sort_lists(expr: TypedExpr) -> TypedExpr {
    let new_cat = match expr.expr {
        ExprCategory::List(subexprs) => {
            let sorted_subexprs = sort(subexprs, &expr.ty);
            ExprCategory::List(sorted_subexprs)
        }

        ExprCategory::In(a, b) =>
            ExprCategory::In(Box::new(sort_lists(*a)), Box::new(sort_lists(*b))),

        ExprCategory::SetOp(op, a, b) =>
            ExprCategory::SetOp(op, Box::new(sort_lists(*a)), Box::new(sort_lists(*b))),

        ExprCategory::And(subexprs) => {
            let subexprs = subexprs.into_iter().map(|x| sort_lists(x)).collect();
            ExprCategory::And(subexprs)
        }

        ExprCategory::Or(subexprs) => {
            let subexprs = subexprs.into_iter().map(|x| sort_lists(x)).collect();
            ExprCategory::Or(subexprs)
        }

        ExprCategory::Not(subexpr) => {
            let subexpr = Box::new(sort_lists(*subexpr));
            ExprCategory::Not(subexpr)
        }

        _ => expr.expr
    };
    return TypedExpr { expr: new_cat, .. expr };
}


fn sort(exprs: Vec<TypedExpr>, list_ty: &Type) -> Vec<TypedExpr> {
    if is_list_of(list_ty, Type::Int) {
        return sort_ints(exprs);
    } else if is_list_of(list_ty, Type::Str) {
        return sort_strs(exprs);
    } else {
        unreachable!()
    }
}


fn is_list_of(ty: &Type, elem_ty: Type) -> bool {
    match *ty {
        Type::List(ref t) => **t == elem_ty,
        _ => false
    }
}


fn sort_ints(exprs: Vec<TypedExpr>) -> Vec<TypedExpr> {
    let mut vals = Vec::with_capacity(exprs.len());
    for expr in exprs {
        if let ExprCategory::Int(x) = expr.expr {
            vals.push(x);
        }
    }
    vals.sort();
    return vals.into_iter()
        .map(|x| TypedExpr {
            expr: ExprCategory::Int(x),
            pos: 0,
            ty: Type::Int
        })
        .collect();
}


fn sort_strs(exprs: Vec<TypedExpr>) -> Vec<TypedExpr> {
    let mut vals = Vec::with_capacity(exprs.len());
    for expr in exprs {
        if let ExprCategory::Str(x) = expr.expr {
            vals.push(x);
        }
    }
    vals.sort();
    return vals.into_iter()
        .map(|x| TypedExpr {
            expr: ExprCategory::Str(x),
            pos: 0,
            ty: Type::Str
        })
        .collect();
}


#[test]
fn test_sort_lists() {
    assert_eq!("n in (2, 3, 5, 7)", test::sort(b"n in (3, 2, 7, 5)"));
    assert_eq!("s in ('a', 'b', 'c')", test::sort(b"s in ('b', 'c', 'a')"));
    assert_eq!("il one of (2, 3, 5)", test::sort(b"il one of (3, 2, 5)"));
    assert_eq!("sl one of ('a', 'b', 'c')", test::sort(b"sl one of ('c', 'a', 'b')"));
    assert_eq!("not n in (2, 3, 5, 7)", test::sort(b"not n in (3, 2, 7, 5)"));
    assert_eq!("(il none of (2, 3, 5) or sl none of ('a', 'b', 'c'))",
               test::sort(b"il none of (3, 2, 5) or sl none of ('c', 'a', 'b')"));
    assert_eq!("(il one of (2, 3, 5) or sl one of ('a', 'b', 'c'))",
               test::sort(b"il one of (3, 2, 5) or sl one of ('c', 'a', 'b')"));
    assert_eq!("(il all of (2, 3, 5) or sl all of ('a', 'b', 'c'))",
               test::sort(b"il all of (3, 2, 5) or sl all of ('c', 'a', 'b')"));
}



#[cfg(test)]
mod test {
    use parser::Parser;
    use scanner::Scanner;
    use typechecker;
    use types::{Nullable, Type, Symtable};

    pub fn sort(input: &[u8]) -> String {
        let mut st = Symtable::new();
        st.add("n", Type::Int, Nullable::No);
        st.add("s", Type::Str, Nullable::No);
        st.add("il", Type::List(Box::new(Type::Int)), Nullable::No);
        st.add("sl", Type::List(Box::new(Type::Str)), Nullable::No);

        let toks = Scanner::scan(input.to_vec()).expect("scanning failed");
        let expr = Parser::parse(toks).expect("parsing failed");
        let texpr = typechecker::typecheck(expr, &st).expect("typechecking failed");
        let texpr = super::sort_lists(texpr);
        return format!("{}", texpr);
    }
}
