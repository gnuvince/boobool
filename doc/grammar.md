The grammar for boobool is "looser" than the one for rtb-boolean;
some constructs that rtb-boolean would refuse, such as `x = y`
or `x not in y` are accepted by boobool.

The grammar below was crafted to be relatively easy to implement
in a hand-written scanner and parser.  Specifically, the grammar
has been written to have no left recursion.

The syntax of rtb-boolean makes some constructs difficult to implement.
For instance, because an expression can be parenthesized and lists use
parentheses to enclose their elements, we don't know if an opening
parenthesis denotes an expression or a list; in addition, is `(1)`
the expression `1` in parentheses or the list containing the element
`1`?  The definitions below thus disallow having a compound expression
start with a list.  `(1, 2, 3) one of (3, 4, 5)` is invalid, however
`x one of (3, 4, 5)` is fine.


```
Expr ::= OrExpr

OrExpr ::= AndExpr { "or" AndExpr }

AndExpr ::= Simple { "and" Simple }

Simple ::= Var "(" Op { "," Op } ")"
         | Var "is" "null"
         | Var "is" "not" "null"
         | "(" OrExpr ")"
         | "not" Simple
         | Op  "in"  Op
         | Op  "not"  "in"  Op
         | Op  "none" "of"  Op
         | Op  "one"  "of"  Op
         | Op  "all"  "of"  Op
         | Op  "="  Op
         | Op  "<>" Op
         | Op  "<"  Op
         | Op  "<=" Op
         | Op  ">"  Op
         | Op  ">=" Op
         | Op

Op ::= Var
     | Literal
     | List

Literal ::= Integer
          | Float
          | String

List ::= "(" Literal { "," Literal } ")"
```
