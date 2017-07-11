# Boolean Expression Compiler

## Language Specification

### Lexer

```nocode
StringLiteral               := '"' ([^\\"] | '\'[\\"])* '"'
                             | "'" ([^\\'] | "\"[\\'])* "'"
Identifier                  := [A-Za-z_][A-Za-z0-9_]*

-- numbers
Sign                        := [+-]
Integer                     := [0-9]+
IntLiteral                  := Sign? Integer

-- floating point numbers
FloatLiteral                := Sign? Integer ("." Integer)
```

### Parser

```nocode
Expression                  := BooleanExpression
BooleanExpression           := OrExpression
OrExpression                := OrExpression ("or" AndExpression)?
AndExpression               := AndExpression "and" NotExpression
NotExpression               := "not"? SimpleExpression
SimpleExpression            := InExpression
                             | NotInExpression
                             | OneOfExpression
                             | NoneOfExpression
                             | AllOfExpression
                             | BinaryComparisonExpression
                             | NumericComparisonExpression
                             | ApplicationExpression
                             | IdentityExpression
                             | TrueExpression
                             | FalseExpression
                             | IsNullExpression
                             | IsNotNullExpression
                             | "(" Expression ")"
InExpression                := Identifier "in" ListExpression
                             | Operand "in" Identifier
NotInExpression             := Identifier "not in" ListExpression
                             | Operand "not in" Identifier
OneOfExpression             := Identifier "one of" ListExpression
NoneOfExpression            := Identifier "none of" ListExpression
AllOfExpression             := Identifier "all of" ListExpression
BinaryComparisonExpression  := StringLiteral EqualityComp Identifier
                             | Identifier EqualityComp StringLiteral
NumericComparisonExpression := Identifier (EqualityComp | ComparisonComp) Number
                             | Number (EqualityComp | ComparisonComp) Identifier
ApplicationExpression       := Identifier ParamList
IdentityExpression          := Identifier
TrueExpression              := "true"
FalseExpression             := "false"
IsNullExpression            := Identifier "is null"
IsNotNullExpression         := Identifier "is not null"
EqualityComp                := = | <>
ComparisonComp              := < | <= | >= | >
ParamList                   := "(" ParamElements ")"
ParamElements               := Param ("," Param)*
Param                       := Operand | Identifier
Number                      := IntLiteral | FloatLiteral
Operand                     := Number | StringLiteral
ListExpression              := "(" (IntLiterals | StringLiterals) ")"
IntLiterals                 := IntLiteral ("," IntLiteral)*
StringLiterals              := StringLiteral ("," StringLiteral)*
```


### Type rules

This section describes the typing rules for the boolean language.
If you already know how to read inference rules, you may skip ahead
to the rules.

We express an `if/then` relation using an horizontal bar.  If the
proposition above the line is true, then the conclusion below the line
is also true.  If there is nothing above the line, the conclusion is
always true (i.e., it is an axiom).  We express a conjunction by
writing multiple propositions separated by spaces (if you want to be
more explicit, you can use the ∧ operator).

**Example:**

```nocode
X > Y    Y > Z
--------------
     X > Z

In English: if X is greater than Y and Y is greater than Z, then X is greater than Z.
```

The proposition `e : T` means that the expression `e` has type `T`.

Rules often need to refer to an environment, a table that maps
an identifier to a type.  It is common to use the greek letter
gamma (Γ) for this purpose.

The turn-stile (⊢) means "it is provable".  For example, the
expression `Γ ⊢ e : T` is read as follows: "under the environment
gamma, it is provable that e has type T."

**Complete example:**

Here is a simple typing rule:

```nocode
Γ ⊢ e1 : int    Γ ⊢ e2 : int
-----------------------------
    Γ ⊢ e1 + e2 : int
```

We read this as follows: "if it is provable under the environment
gamma that e1 has type int and it is provable under the same
environment gamma that e2 has type int, then it is provable under
gamma that the addition of e1 and e2 has type int."


#### Literals

```nocode
-----------    ------------
true : bool    false : bool


n is IntLiteral    f is FloatLiteral   s is StringLiteral
---------------    -----------------   ------------------
   n : int             f : float             s : str


l is IntList   l is StringList
------------   ---------------
l : int list    l : str list
```

#### Operators

```nocode
  v is a variable           v is a variable
--------------------    ------------------------
Γ ⊢ v is null : bool    Γ ⊢ v is not null : bool



Γ ⊢ e1 : T    Γ ⊢ e2 : T list   T ∈ [int, str]
-----------------------------------------------
        Γ ⊢ e1 (not) in e2 : bool



Γ ⊢ e1 : T    Γ ⊢ e2 : T list    T ∈ [int, str]
-----------------------------------------------
            Γ ⊢ e1 none of e2 : bool
                   one of
                   all of



Γ ⊢ e1 : T1    Γ ⊢ e2 : T2    T1 ∈ [int, float]    T2 ∈ [int, float]
--------------------------------------------------------------------
            Γ ⊢ e1  =  e2 : bool
                    <>
                    <
                    <=
                    >
                    >=



Γ ⊢ e1 : str    Γ ⊢ e2 : str
----------------------------
    Γ ⊢ e1  =  e2 : bool
            <>
```


#### Functions

```nocode
Γ ⊢ e1 : str    Γ ⊢ e2 : str    Γ ⊢ e3 : int    Γ ⊢ e4 : int
-------------------------------------------------------------
    Γ ⊢ within_frequency_cap(e1, e2, e3, e4) : bool



Γ ⊢ e1 : T    Γ ⊢ e2 : T    Γ ⊢ e3 : T    T ∈ [int, float]
-----------------------------------------------------------
    Γ ⊢ geo_within_radius(e1, e2, e3) : bool




   Γ ⊢ e1 : int    Γ ⊢ e2 : int
-----------------------------------
Γ ⊢ segments_before(e1, e2) : bool



   Γ ⊢ e1 : int    Γ ⊢ e2 : int
-----------------------------------
Γ ⊢ segments_within(e1, e2) : bool



   Γ ⊢ e1 : str    Γ ⊢ e2 : str
-----------------------------------
  Γ ⊢ starts_with(e1, e2) : bool



   Γ ⊢ e1 : str    Γ ⊢ e2 : str
-----------------------------------
   Γ ⊢ ends_with(e1, e2) : bool
```
