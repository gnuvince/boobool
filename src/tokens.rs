use std::fmt;

use errors::{Error, Result};

/// The categories of tokens for the Boolean language.
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


impl fmt::Display for TokenCategory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenCategory::Not => write!(f, "'not'"),
            TokenCategory::And => write!(f, "'and'"),
            TokenCategory::Or => write!(f, "'or'"),
            TokenCategory::None => write!(f, "'none'"),
            TokenCategory::One => write!(f, "'one'"),
            TokenCategory::All => write!(f, "'all'"),
            TokenCategory::Of => write!(f, "'of'"),
            TokenCategory::Is => write!(f, "'is'"),
            TokenCategory::Null => write!(f, "'null'"),
            TokenCategory::In => write!(f, "'in'"),
            TokenCategory::Eq => write!(f, "'='"),
            TokenCategory::Ne => write!(f, "'<>'"),
            TokenCategory::Lt => write!(f, "'<'"),
            TokenCategory::Le => write!(f, "'<='"),
            TokenCategory::Gt => write!(f, "'>'"),
            TokenCategory::Ge => write!(f, "'>='"),
            TokenCategory::LParen => write!(f, "'('"),
            TokenCategory::RParen => write!(f, "')'"),
            TokenCategory::Comma => write!(f, "','"),
            TokenCategory::IntLiteral => write!(f, "'int literal'"),
            TokenCategory::StrLiteral => write!(f, "'str literal'"),
            TokenCategory::FloatLiteral => write!(f, "'float literal'"),
            TokenCategory::Var => write!(f, "'var'"),
            TokenCategory::Eof => write!(f, "<eof>"),
        }
    }
}


/// A token of the Boolean language.  A token has a
/// category, an optional lexeme (the string that yielded
/// the token) and an offset (position).
#[derive(Debug, PartialEq)]
pub struct Token {
    pub cat: TokenCategory,
    pub lexeme: Option<String>,
    pub offset: usize,
}


impl Token {
    /// Creates a new token object.
    pub fn new(cat: TokenCategory, lexeme: Option<String>, offset: usize)
               -> Token {
        return Token { cat, lexeme, offset };
    }


    /// Returns the lexeme of a token as a string.
    pub fn lexeme_string(&self) -> Result<String> {
        self
            .lexeme
            .clone()
            .ok_or(Error::MissingLexeme(self.offset))
    }


    /// Returns the lexeme of a token as an `i64`.
    pub fn lexeme_i64(&self) -> Result<i64> {
        let s = self.lexeme_string()?;
        return s.parse::<i64>().map_err(|_| Error::InvalidIntLiteral(self.offset));
    }


    /// Returns the lexeme of a token as an `f64`.
    pub fn lexeme_f64(&self) -> Result<f64> {
        let s = self.lexeme_string()?;
        return s.parse::<f64>().map_err(|_| Error::InvalidFloatLiteral(self.offset));
    }
}
