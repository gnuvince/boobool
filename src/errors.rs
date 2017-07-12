use std::fmt;
use std::error::Error as E;

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    // Scanner errors
    UnknownCharacter(u8, usize),
    UnterminatedStringLiteral(usize),
    MissingDigits(usize),

    // Parser errors
    MissingLexeme(usize),
    InvalidIntLiteral(usize),
    InvalidFloatLiteral(usize),
    MissingComma(usize),
    ExtraTokens(usize),
    InvalidToken(usize),
    UnterminatedList(usize),
    InvalidOperation(usize),
    InvalidOperator(usize),
}


impl Error {
    fn offset(&self) -> Result<usize> {
        match *self {
            Error::UnknownCharacter(_, x) => Ok(x),
            Error::UnterminatedStringLiteral(x) => Ok(x),
            Error::MissingDigits(x) => Ok(x),
            Error::MissingLexeme(x) => Ok(x),
            Error::InvalidIntLiteral(x) => Ok(x),
            Error::InvalidFloatLiteral(x) => Ok(x),
            Error::MissingComma(x) => Ok(x),
            Error::ExtraTokens(x) => Ok(x),
            Error::InvalidToken(x) => Ok(x),
            Error::UnterminatedList(x) => Ok(x),
            Error::InvalidOperation(x) => Ok(x),
            Error::InvalidOperator(x) => Ok(x),
        }
    }
}


impl E for Error {
    fn description(&self) -> &'static str {
        match *self {
            Error::UnknownCharacter(_, _) => "unknown character",
            Error::UnterminatedStringLiteral(_) => "unterminated string literal",
            Error::MissingDigits(_) => "missing digits",
            Error::MissingLexeme(_) => "missing lexeme",
            Error::InvalidIntLiteral(_) => "invalid int literal",
            Error::InvalidFloatLiteral(_) => "invalid float literal",
            Error::MissingComma(_) => "missing comma",
            Error::ExtraTokens(_) => "extra tokens",
            Error::InvalidToken(_) => "invalid token",
            Error::UnterminatedList(_) => "unterminated list",
            Error::InvalidOperation(_) => "invalid operation",
            Error::InvalidOperator(_) => "invalid operator",
        }
    }
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())?;
        if let Ok(offset) = self.offset() {
            write!(f, " at offset {}", offset)?;
        }
        return Ok(());
    }
}
