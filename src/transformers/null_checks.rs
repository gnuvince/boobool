use std::collections::HashSet;

use ast::{ExprCategory, TypedExpr};
use types::{Nullable, Type, Symtable};


pub fn insert_null_checks(expr: TypedExpr, st: &Symtable) -> TypedExpr {
    let (nullables, rewritten_expr) = rewrite(expr, st);
    make_checks(nullables, rewritten_expr)
}


// TODO(vfoley): reduce the amount of repeated code
fn rewrite(expr: TypedExpr, st: &Symtable) -> (HashSet<String>, TypedExpr) {
    match expr.expr {
        ExprCategory::Var(s) => {
            let new_expr = TypedExpr {
                expr: ExprCategory::Var(s.clone()),
                pos: 0,
                ty: expr.ty
            };
            match st.get_nullable(&s) {
                None => (HashSet::new(), new_expr),
                Some(Nullable::No) => (HashSet::new(), new_expr),
                Some(Nullable::Yes) => {
                    let mut set = HashSet::new();
                    set.insert(s);
                    (set, new_expr)
                }
            }
        }

        ExprCategory::Compare(op, a, b) => {
            let (mut nullables_a, a) = rewrite(*a, st);
            let (nullables_b, b) = rewrite(*b, st);
            nullables_a.extend(nullables_b);
            (nullables_a, TypedExpr {
                expr: ExprCategory::Compare(op, Box::new(a), Box::new(b)),
                .. expr
            })
        }

        ExprCategory::In(a, b) => {
            let (mut nullables_a, a) = rewrite(*a, st);
            let (nullables_b, b) = rewrite(*b, st);
            nullables_a.extend(nullables_b);
            (nullables_a, TypedExpr {
                expr: ExprCategory::In(Box::new(a), Box::new(b)),
                .. expr
            })
        }

        ExprCategory::Call(func_name, args) => {
            let mut nullables = HashSet::new();
            let mut rewritten_args = Vec::new();
            for arg in args {
                let (nullables_arg, arg) = rewrite(arg, st);
                rewritten_args.push(arg);
                nullables.extend(nullables_arg);
            }
            (nullables, TypedExpr {
                expr: ExprCategory::Call(func_name, rewritten_args),
                .. expr
            })
        }

        ExprCategory::Or(subexprs) => {
            let mut nullables = HashSet::new();
            let mut rewritten_subexprs = Vec::new();
            for subexpr in subexprs {
                let (nullables_subexpr, subexpr) = rewrite(subexpr, st);
                rewritten_subexprs.push(subexpr);
                nullables.extend(nullables_subexpr);
            }
            let rewritten_expr = TypedExpr {
                expr: ExprCategory::Or(rewritten_subexprs),
                .. expr
            };
            if nullables.is_empty() {
                (HashSet::new(), rewritten_expr)
            } else {
                (HashSet::new(), make_checks(nullables, rewritten_expr))
            }
        }

        ExprCategory::And(subexprs) => {
            let mut nullables = HashSet::new();
            let mut rewritten_subexprs = Vec::new();
            for subexpr in subexprs {
                let (nullables_subexpr, subexpr) = rewrite(subexpr, st);
                rewritten_subexprs.push(subexpr);
                nullables.extend(nullables_subexpr);
            }
            let rewritten_expr = TypedExpr {
                expr: ExprCategory::And(rewritten_subexprs),
                .. expr
            };
            (HashSet::new(), make_checks(nullables, rewritten_expr))
        }

        ExprCategory::Not(subexpr) => {
            let (nullables, subexpr) = rewrite(*subexpr, st);
            let rewritten_expr = TypedExpr {
                expr: ExprCategory::Not(Box::new(subexpr)),
                .. expr
            };
            (HashSet::new(), make_checks(nullables, rewritten_expr))
        }

        _ => (HashSet::new(), expr)
    }
}


fn make_checks(nullables: HashSet<String>, expr: TypedExpr) -> TypedExpr {
    if nullables.is_empty() {
        return expr;
    }

    let mut clauses = Vec::new();
    for nullable in nullables {
        clauses.push(TypedExpr {
            expr: ExprCategory::Not(Box::new(TypedExpr {
                expr: ExprCategory::IsNull(nullable),
                pos: 0,
                ty: Type::Bool
            })),
            pos: 0,
            ty: Type::Bool
        });
    }
    clauses.push(expr);
    return TypedExpr {
        expr: ExprCategory::And(clauses),
        pos: 0,
        ty: Type::Bool
    };
}



#[test]
fn test_insert_null_checks() {
    assert_eq!("n = 3", test::null_check(b"n = 3"));
    assert_eq!("(not opt_n is null and opt_n = 3)", test::null_check(b"opt_n = 3"));
    assert_eq!("b", test::null_check(b"b"));
    assert_eq!("(not opt_b is null and opt_b)", test::null_check(b"opt_b"));
    assert_eq!("(not opt_b is null and (opt_b and opt_b))",
               test::null_check(b"opt_b and opt_b"));
    assert_eq!("s in ('a', 'b')", test::null_check(b"s in ('a', 'b')"));
    assert_eq!("(not opt_s is null and opt_s in ('a', 'b'))",
               test::null_check(b"opt_s in ('a', 'b')"));
    assert_eq!("(not opt_n is null and not opt_n = 3)",
               test::null_check(b"not opt_n = 3"));
}


#[cfg(test)]
mod test {
    use parser::Parser;
    use scanner::Scanner;
    use typechecker;
    use types::{Nullable, Type, Symtable};

    pub fn null_check(input: &[u8]) -> String {
        let mut st = Symtable::new();
        st.add("b", Type::Bool, Nullable::No);
        st.add("opt_b", Type::Bool, Nullable::Yes);
        st.add("n", Type::Int, Nullable::No);
        st.add("opt_n", Type::Int, Nullable::Yes);
        st.add("s", Type::Str, Nullable::No);
        st.add("opt_s", Type::Str, Nullable::Yes);
        st.add("il", Type::List(Box::new(Type::Int)), Nullable::No);
        st.add("opt_il", Type::List(Box::new(Type::Int)), Nullable::Yes);
        st.add("sl", Type::List(Box::new(Type::Str)), Nullable::No);
        st.add("opt_sl", Type::List(Box::new(Type::Str)), Nullable::Yes);

        let toks = Scanner::scan(input.to_vec()).expect("scanning failed");
        let expr = Parser::parse(toks).expect("parsing failed");
        let texpr = typechecker::typecheck(expr, &st).expect("typechecking failed");
        let texpr = super::insert_null_checks(texpr, &st);
        return format!("{}", texpr);
    }
}
