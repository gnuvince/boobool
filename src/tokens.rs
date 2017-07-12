use errors::{Error, Result};

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

    Eof
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


    pub fn lexeme_string(&self) -> Result<String> {
        self
            .lexeme
            .clone()
            .ok_or(Error::MissingLexeme(self.offset))
    }

    pub fn lexeme_i64(&self) -> Result<i64> {
        let s = self.lexeme_string()?;
        return s.parse::<i64>().map_err(|_| Error::InvalidIntLiteral(self.offset));
    }

    pub fn lexeme_f64(&self) -> Result<f64> {
        let s = self.lexeme_string()?;
        return s.parse::<f64>().map_err(|_| Error::InvalidFloatLiteral(self.offset));
    }
}
