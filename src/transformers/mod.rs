use ast::TypedExpr;
use types::Symtable;

mod ors;
mod sort_lists;

pub fn transform(mut expr: TypedExpr, st: &Symtable) -> TypedExpr {
    expr = ors::transform_ors(expr, st);
    expr = sort_lists::sort_lists(expr);
    return expr;
}
