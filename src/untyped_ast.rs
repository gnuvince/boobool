use std::fmt;

#[derive(Debug)]
pub enum Expr {
    // Base expressions
    Var(String),
    Int(i64),
    Float(f64),
    Str(String),
    List(Vec<Expr>),

    // Operations
    Compare(CmpOp, Box<Expr>, Box<Expr>),
    In(Box<Expr>, Box<Expr>),
    NoneOf(Box<Expr>, Box<Expr>),
    OneOf(Box<Expr>, Box<Expr>),
    AllOf(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    IsNull(String), // Only valid on variable names

    // Boolean combinators
    Or(Vec<Expr>),
    And(Vec<Expr>),
    Not(Box<Expr>),
}


#[derive(Debug, Clone, Copy)]
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


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Var(ref v) => write!(f, "{}", v),
            Int(i) => write!(f, "{}", i),
            Float(x) => write!(f, "{}", x),
            Str(ref s) => write!(f, "'{}'", s),
            List(ref elems) => {
                let subexprs: Vec<String> = elems.iter().map(|x| format!("{}", x)).collect();
                write!(f, "({})", subexprs.join(", "))
            }
            Compare(op, ref a, ref b) => write!(f, "{} {} {}", a, op, b),
            In(ref needle, ref haystack) =>
                write!(f, "{} in {}", needle, haystack),
            NoneOf(ref a, ref b) =>
                write!(f, "{} none of {}", a, b),
            OneOf(ref a, ref b) =>
                write!(f, "{} one of {}", a, b),
            AllOf(ref a, ref b) =>
                write!(f, "{} all of {}", a, b),
            Call(ref func, ref args) => {
                let subexprs: Vec<String> = args.iter().map(|x| format!("{}", x)).collect();
                write!(f, "{}({})", func, subexprs.join(", "))
            }
            IsNull(ref var) => write!(f, "{} is null", var),
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
