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

    // Type checker errors
    EmptyList,
    IncorrectType,
    InvalidListType,
    InvalidComparison,
    InvalidSetOperation,
    NotAFunction,

    // Symbol table errors
    UndeclaredVariable,
}


impl Error {
    fn offset(&self) -> Option<usize> {
        match *self {
            Error::UnknownCharacter(_, x) => Some(x),
            Error::UnterminatedStringLiteral(x) => Some(x),
            Error::MissingDigits(x) => Some(x),
            Error::MissingLexeme(x) => Some(x),
            Error::InvalidIntLiteral(x) => Some(x),
            Error::InvalidFloatLiteral(x) => Some(x),
            Error::MissingComma(x) => Some(x),
            Error::ExtraTokens(x) => Some(x),
            Error::InvalidToken(x) => Some(x),
            Error::UnterminatedList(x) => Some(x),
            Error::InvalidOperation(x) => Some(x),
            Error::InvalidOperator(x) => Some(x),

            _ => None
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
            Error::EmptyList => "cannot type an empty list",

            Error::IncorrectType => "incorrect type",
            Error::InvalidListType => "only lists of ints and strings are allowed",
            Error::InvalidComparison => "invalid comparison",
            Error::InvalidSetOperation => "invalid set operation",
            Error::NotAFunction => "not a function",

            Error::UndeclaredVariable => "undeclared variable",
        }
    }
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())?;
        if let Some(offset) = self.offset() {
            write!(f, " at offset {}", offset)?;
        }
        return Ok(());
    }
}
