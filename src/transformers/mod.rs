use ast::TypedExpr;
use types::Symtable;

mod ors;
mod sort_lists;
mod null_checks;

pub fn transform(mut expr: TypedExpr, st: &Symtable) -> TypedExpr {
    expr = ors::transform_ors(expr, st);
    expr = sort_lists::sort_lists(expr);
    expr = null_checks::insert_null_checks(expr, st);
    return expr;
}


#[test]
fn test_transform() {
    assert_eq!("s in ('A', 'B')", test::transform(b"s = 'B' or s = 'A'"));
}


#[cfg(test)]
mod test {
    use parser::Parser;
    use scanner::Scanner;
    use typechecker;
    use types::{Nullable, Type, Symtable};

    pub fn transform(input: &[u8]) -> String {
        let mut st = Symtable::new();
        st.add("n", Type::Int, Nullable::No);
        st.add("s", Type::Str, Nullable::No);
        st.add("il", Type::List(Box::new(Type::Int)), Nullable::No);
        st.add("sl", Type::List(Box::new(Type::Str)), Nullable::No);

        let toks = Scanner::scan(input.to_vec()).expect("scanning failed");
        let expr = Parser::parse(toks).expect("parsing failed");
        let texpr = typechecker::typecheck(expr, &st).expect("typechecking failed");
        let texpr = super::transform(texpr, &st);
        return format!("{}", texpr);
    }
}
