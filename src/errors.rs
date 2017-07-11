pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    UnknownCharacter(u8, usize),
    UnterminatedStringLiteral(usize),
    MissingDigits(usize),
}
