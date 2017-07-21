use std::fmt;
use std::error::Error as E;

use types::Type;

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    // Scanner errors
    UnknownCharacter(usize, u8),
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
    ExpressionMustBeBool(usize),
    EmptyList(usize),
    IncorrectType(usize, Type, Type),
    InvalidListType(usize, Type),
    InvalidSetOperation(usize),
    NotAFunction(usize, String),
    IncorrectArgListLength(usize, usize, usize),

    // Symbol table errors
    UndeclaredVariable(usize, String),

    // Environment validation errors
    CannotBeNull(String),
    EnvInvalidType(String, Type, Type),
}


impl Error {
    fn offset(&self) -> Option<usize> {
        match *self {
            Error::UnknownCharacter(x, _)
            | Error::UnterminatedStringLiteral(x)
            | Error::MissingDigits(x)
            | Error::MissingLexeme(x)
            | Error::InvalidIntLiteral(x)
            | Error::InvalidFloatLiteral(x)
            | Error::MissingComma(x)
            | Error::ExtraTokens(x)
            | Error::InvalidToken(x)
            | Error::UnterminatedList(x)
            | Error::InvalidOperation(x)
            | Error::InvalidOperator(x)
            | Error::ExpressionMustBeBool(x)
            | Error::EmptyList(x)
            | Error::IncorrectType(x, _, _)
            | Error::InvalidListType(x, _)
            | Error::InvalidSetOperation(x)
            | Error::NotAFunction(x, _)
            | Error::UndeclaredVariable(x, _)
            | Error::IncorrectArgListLength(x, _, _) => Some(x),
            Error::CannotBeNull(_) => None,
            Error::EnvInvalidType(_, _, _) => None,
        }
    }

    fn extra_info(&self) -> String {
        match *self {
            Error::UnknownCharacter(_, c) =>
                format!("'{}'", c as char),
            Error::IncorrectType(_, ref t1, ref t2) =>
                format!("expected {}, found {}", t1, t2),
            Error::InvalidListType(_, ref t) =>
                format!("cannot use {} in a list", t),
            Error::NotAFunction(_, ref name) =>
                format!("{}", name),
            Error::IncorrectArgListLength(_, exp, act) =>
                format!("expected {}, found {}", exp, act),
            Error::UndeclaredVariable(_, ref name) =>
                format!("{}", name),
            Error::CannotBeNull(ref name) =>
                format!("{}", name),
            Error::EnvInvalidType(ref var, ref expected, ref actual) =>
                format!("variable {}: expected {}, found {}",
                        var, expected, actual),
            _ =>
                String::new()
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

            Error::ExpressionMustBeBool(_) => "expression must have type bool",
            Error::EmptyList(_) => "cannot type an empty list",
            Error::IncorrectType(_, _, _) => "incorrect type",
            Error::InvalidListType(_, _) => "only lists of ints and strings are allowed",
            Error::InvalidSetOperation(_) => "invalid set operation",
            Error::NotAFunction(_, _) => "not a function",
            Error::IncorrectArgListLength(_, _, _) => "incorrect number of arguments",

            Error::UndeclaredVariable(_, _) => "undeclared variable",

            Error::CannotBeNull(_) => "variable cannot be null",
            Error::EnvInvalidType(_, _, _) => "invalid type for environment variable",
        }
    }
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())?;
        if let Some(offset) = self.offset() {
            write!(f, " at position {}", offset+1)?;
        }
        let extra = self.extra_info();
        if !extra.is_empty() {
            write!(f, ": {}", extra)?;
        }
        return Ok(());
    }
}
