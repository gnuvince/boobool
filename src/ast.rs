use std::fmt;

use types::Type;


pub type UntypedExpr = Expr<()>;
pub type TypedExpr = Expr<Type>;


#[derive(Debug)]
pub struct Expr<T> {
    pub expr: ExprCategory<T>,
    pub pos: usize, // use `pos` here to not be confused with `offset` in the parser
    pub ty: T,
}


#[derive(Debug)]
pub enum ExprCategory<T> {
    // Base expressions
    Var(String),
    Int(i64),
    Float(f64),
    Str(String),
    List(Vec<Expr<T>>),

    // Operations
    Compare(CmpOp, Box<Expr<T>>, Box<Expr<T>>),
    In(Box<Expr<T>>, Box<Expr<T>>),
    SetOp(SetOp, Box<Expr<T>>, Box<Expr<T>>),
    Call(String, Vec<Expr<T>>),
    IsNull(String), // Only valid on variable names

    // Boolean combinators
    Or(Vec<Expr<T>>),
    And(Vec<Expr<T>>),
    Not(Box<Expr<T>>),
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CmpOp {
    Eq, Ne, Lt, Le, Gt, Ge
}


impl fmt::Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CmpOp::Eq => f.write_str("="),
            CmpOp::Ne => f.write_str("<>"),
            CmpOp::Lt => f.write_str("<"),
            CmpOp::Le => f.write_str("<="),
            CmpOp::Gt => f.write_str(">"),
            CmpOp::Ge => f.write_str(">="),
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SetOp {
    NoneOf, OneOf, AllOf
}


impl fmt::Display for SetOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SetOp::NoneOf => write!(f, "none of"),
            SetOp::OneOf => write!(f, "one of"),
            SetOp::AllOf => write!(f, "all of"),
        }
    }
}


impl <T> fmt::Display for ExprCategory<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ExprCategory::*;
        match *self {
            Var(ref v) =>
                write!(f, "{}", v),
            Int(i) =>
                write!(f, "{}", i),
            Float(x) =>
                write!(f, "{}", x),
            Str(ref s) =>
                write!(f, "'{}'", s),
            List(ref elems) => {
                let subexprs: Vec<String> = elems.iter().map(|x| format!("{}", x)).collect();
                write!(f, "({})", subexprs.join(", "))
            }
            Compare(op, ref a, ref b) =>
                write!(f, "{} {} {}", a, op, b),
            In(ref needle, ref haystack) =>
                write!(f, "{} in {}", needle, haystack),
            SetOp(op, ref a, ref b) =>
                write!(f, "{} {} {}", a, op, b),
            Call(ref func, ref args) => {
                let subexprs: Vec<String> = args.iter().map(|x| format!("{}", x)).collect();
                write!(f, "{}({})", func, subexprs.join(", "))
            }
            IsNull(ref var) =>
                write!(f, "{} is null", var),
            Or(ref exprs) => {
                let subexprs: Vec<String> = exprs.iter().map(|x| format!("{}", x)).collect();
                write!(f, "({})", subexprs.join(" or "))
            }
            And(ref exprs) => {
                let subexprs: Vec<String> = exprs.iter().map(|x| format!("{}", x)).collect();
                write!(f, "({})", subexprs.join(" and "))
            }
            Not(ref expr) => write!(f, "not {}", expr)
        }
    }
}


impl <T> fmt::Display for Expr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}
