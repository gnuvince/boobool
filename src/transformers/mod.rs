use ast::TypedExpr;
use types::Symtable;

mod ors;

pub fn transform(mut expr: TypedExpr, st: &Symtable) -> TypedExpr {
    expr = ors::transform_ors(expr, st);
    return expr;
}
