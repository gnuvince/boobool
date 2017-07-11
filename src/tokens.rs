#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenCategory {
    // Keywords
    Not, And, Or, None, One, All, Of, Is, Null, In,

    // Punctation and operators
    Eq, Ne, Lt, Le, Gt, Ge, LParen, RParen, Comma,

    // Literals
    IntLiteral, StrLiteral, FloatLiteral,

    // Variables
    Var,

    EOF
}


#[derive(Debug, PartialEq)]
pub struct Token {
    pub cat: TokenCategory,
    pub lexeme: Option<String>,
    pub offset: usize,
}


impl Token {
    pub fn new(cat: TokenCategory, lexeme: Option<String>, offset: usize)
               -> Token {
        return Token { cat, lexeme, offset };
    }
}
