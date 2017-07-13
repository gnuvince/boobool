The grammar for boobool is "looser" than the one for rtb-boolean;
some constructs that rtb-boolean would refuse, such as `x = y`
or `x not in y` are accepted by boobool.

The grammar below was crafted to be relatively easy to implement
in a hand-written scanner and parser.


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
