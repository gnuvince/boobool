use tokens::Token;
use tokens::TokenCategory as TC;
use errors::{Error, Result};

pub struct Scanner {
    src: Vec<u8>,
    offset: usize,
}


impl Scanner {
    /// Scans
    pub fn scan(src: Vec<u8>) -> Result<Vec<Token>> {
        let mut scanner = Scanner { src: src, offset: 0 };
        return scanner.scan_expr();
    }


    fn scan_expr(&mut self) -> Result<Vec<Token>> {
        let mut toks: Vec<Token> = Vec::new();
        while !self.eof() {
            while is_whitespace(self.peek()) {
                self.offset += 1;
                continue;
            }
            toks.push(self.next()?);
        }
        toks.push(Token::new(TC::EOF, None, self.offset));
        return Ok(toks);
    }


    fn next(&mut self) -> Result<Token> {
        if self.looking_at(b"(")    { return Ok(Token::new(TC::LParen, None, self.offset)); }
        if self.looking_at(b")")    { return Ok(Token::new(TC::RParen, None, self.offset)); }
        if self.looking_at(b"<>")   { return Ok(Token::new(TC::Ne, None, self.offset)); }
        if self.looking_at(b"<=")   { return Ok(Token::new(TC::Le, None, self.offset)); }
        if self.looking_at(b">=")   { return Ok(Token::new(TC::Ge, None, self.offset)); }
        if self.looking_at(b"=")    { return Ok(Token::new(TC::Eq, None, self.offset)); }
        if self.looking_at(b"<")    { return Ok(Token::new(TC::Lt, None, self.offset)); }
        if self.looking_at(b">")    { return Ok(Token::new(TC::Gt, None, self.offset)); }
        if self.looking_at(b",")    { return Ok(Token::new(TC::Comma, None, self.offset)); }
        if self.looking_at(b"not")  { return Ok(Token::new(TC::Not, None, self.offset)); }
        if self.looking_at(b"and")  { return Ok(Token::new(TC::And, None, self.offset)); }
        if self.looking_at(b"or")   { return Ok(Token::new(TC::Or, None, self.offset)); }
        if self.looking_at(b"none") { return Ok(Token::new(TC::None, None, self.offset)); }
        if self.looking_at(b"one")  { return Ok(Token::new(TC::One, None, self.offset)); }
        if self.looking_at(b"all")  { return Ok(Token::new(TC::All, None, self.offset)); }
        if self.looking_at(b"of")   { return Ok(Token::new(TC::Of, None, self.offset)); }
        if self.looking_at(b"is")   { return Ok(Token::new(TC::Is, None, self.offset)); }
        if self.looking_at(b"null") { return Ok(Token::new(TC::Null, None, self.offset)); }
        if self.looking_at(b"in")   { return Ok(Token::new(TC::In, None, self.offset)); }
        if self.peek() == b'"'      { return self.scan_string(b'"'); }
        if self.peek() == b'\''     { return self.scan_string(b'\''); }
        if is_digit(self.peek())    { return self.scan_number(); }
        if is_alpha(self.peek())    { return self.scan_var(); }
        return Err(Error::UnknownCharacter(self.peek(), self.offset));
    }


    fn scan_string(&mut self, closing_quote: u8) -> Result<Token> {
        let mut buf = String::new();
        let initial_offset = self.offset;

        self.offset += 1; // Skip over opening quote
        while !self.eof() && self.peek() != closing_quote {
            buf.push(self.peek() as char);
            self.offset += 1;
        }
        if self.eof() {
            return Err(Error::UnterminatedStringLiteral(initial_offset));
        }
        self.offset += 1; // Skip over closing quote
        return Ok(Token::new(TC::StrLiteral, Some(buf), initial_offset));
    }


    fn scan_var(&mut self) -> Result<Token> {
        let mut buf = String::new();
        let initial_offset = self.offset;

        while !self.eof() && is_alnum(self.peek()) {
            buf.push(self.peek() as char);
            self.offset += 1;
        }

        return Ok(Token::new(TC::Var, Some(buf), initial_offset));
    }


    fn scan_number(&mut self) -> Result<Token> {
        let initial_offset = self.offset;

        let integral_part = self.scan_digits()?;
        if self.looking_at(b".") {
            let decimal_part = self.scan_digits()?;
            let whole = format!("{}.{}", integral_part, decimal_part);
            return Ok(Token::new(TC::FloatLiteral, Some(whole), initial_offset));
        } else {
            return Ok(Token::new(TC::IntLiteral, Some(integral_part), initial_offset));
        }
    }


    fn scan_digits(&mut self) -> Result<String> {
        let mut buf = String::new();
        while !self.eof() && is_digit(self.peek()) {
            buf.push(self.peek() as char);
            self.offset += 1;
        }
        if buf.is_empty() {
            return Err(Error::MissingDigits(self.offset));
        }
        return Ok(buf);
    }


    fn peek(&self) -> u8 {
        return self.peek_at(self.offset);
    }

    fn peek_at(&self, offset: usize) -> u8 {
        if offset >= self.src.len() {
            return 0;
        } else {
            return self.src[offset];
        }
    }

    fn looking_at(&mut self, bytes: &[u8]) -> bool {
        for (i, &b) in bytes.iter().enumerate() {
            if self.peek_at(self.offset + i) != b {
                return false;
            }
        }
        self.offset += bytes.len();
        return true;
    }

    fn eof(&self) -> bool {
        return self.peek() == 0;
    }
}


fn is_whitespace(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\n' || c == b'\r'
}


fn is_alpha(c: u8) -> bool {
    c == b'_' || (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z')
}

fn is_digit(c: u8) -> bool {
    c >= b'0' && c <= b'9'
}

fn is_alnum(c: u8) -> bool {
    is_alpha(c) || is_digit(c)
}



#[test]
fn test_scanner_basic_tokens() {
    assert!(Scanner::scan(b"(".to_vec()).is_ok());
    assert!(Scanner::scan(b")".to_vec()).is_ok());
    assert!(Scanner::scan(b"=".to_vec()).is_ok());
    assert!(Scanner::scan(b"<>".to_vec()).is_ok());
    assert!(Scanner::scan(b"<".to_vec()).is_ok());
    assert!(Scanner::scan(b"<=".to_vec()).is_ok());
    assert!(Scanner::scan(b">".to_vec()).is_ok());
    assert!(Scanner::scan(b">=".to_vec()).is_ok());
    assert!(Scanner::scan(b",".to_vec()).is_ok());
    assert!(Scanner::scan(b"not".to_vec()).is_ok());
    assert!(Scanner::scan(b"and".to_vec()).is_ok());
    assert!(Scanner::scan(b"or".to_vec()).is_ok());
    assert!(Scanner::scan(b"none".to_vec()).is_ok());
    assert!(Scanner::scan(b"one".to_vec()).is_ok());
    assert!(Scanner::scan(b"all".to_vec()).is_ok());
    assert!(Scanner::scan(b"of".to_vec()).is_ok());
    assert!(Scanner::scan(b"is".to_vec()).is_ok());
    assert!(Scanner::scan(b"null".to_vec()).is_ok());
    assert!(Scanner::scan(b"in".to_vec()).is_ok());
    assert!(Scanner::scan(b"x".to_vec()).is_ok());
    assert!(Scanner::scan(b"1".to_vec()).is_ok());
    assert!(Scanner::scan(b"12".to_vec()).is_ok());
    assert!(Scanner::scan(b"0.13".to_vec()).is_ok());
    assert!(Scanner::scan(b"12.34".to_vec()).is_ok());
    assert!(Scanner::scan(b"12.34".to_vec()).is_ok());
    assert!(Scanner::scan(b"''".to_vec()).is_ok());
    assert!(Scanner::scan(b"'hello'".to_vec()).is_ok());
    assert!(Scanner::scan(b"'O\"Reilly'".to_vec()).is_ok());
    assert!(Scanner::scan(b"\"\"".to_vec()).is_ok());
    assert!(Scanner::scan(b"\"hello\"".to_vec()).is_ok());
    assert!(Scanner::scan(b"\"O'Reilly\"".to_vec()).is_ok());
}

#[test]
fn test_scanner_whitespace() {
    assert!(match Scanner::scan(b"var".to_vec()) {
        Ok(toks) => { toks.len() == 2 && toks[0].cat == TC::Var }
        _ => false
    });

    assert!(match Scanner::scan(b" var".to_vec()) {
        Ok(toks) => { toks.len() == 2 && toks[0].cat == TC::Var }
        _ => false
    });

    assert!(match Scanner::scan(b" \n\n\t\t  var".to_vec()) {
        Ok(toks) => { toks.len() == 2 && toks[0].cat == TC::Var }
        _ => false
    });
}


#[test]
fn test_scanner_err() {
    assert!(match Scanner::scan(b"&".to_vec()) {
        Err(Error::UnknownCharacter(b'&', _)) => true,
        _ => false
    });

    assert!(match Scanner::scan(b"'unterminated".to_vec()) {
        Err(Error::UnterminatedStringLiteral(_)) => true,
        _ => false
    });

    assert!(match Scanner::scan(b"\"unterminated".to_vec()) {
        Err(Error::UnterminatedStringLiteral(_)) => true,
        _ => false
    });

    assert!(match Scanner::scan(b"123.".to_vec()) {
        Err(Error::MissingDigits(_)) => true,
        _ => false
    });
}


#[test]
fn test_token_categories() {
    assert_eq!(vec![TC::Var, TC::EOF], token_categories(b"var"));
    assert_eq!(vec![TC::IntLiteral, TC::EOF], token_categories(b"1"));
    assert_eq!(vec![TC::FloatLiteral, TC::EOF], token_categories(b"1.2"));
    assert_eq!(vec![TC::StrLiteral, TC::EOF], token_categories(b"'hello'"));
    assert_eq!(vec![TC::StrLiteral, TC::EOF], token_categories(b"\"hello\""));
    assert_eq!(vec![TC::LParen, TC::EOF], token_categories(b"("));
    assert_eq!(vec![TC::RParen, TC::EOF], token_categories(b")"));
    assert_eq!(vec![TC::Eq, TC::EOF], token_categories(b"="));
    assert_eq!(vec![TC::Ne, TC::EOF], token_categories(b"<>"));
    assert_eq!(vec![TC::Lt, TC::EOF], token_categories(b"<"));
    assert_eq!(vec![TC::Le, TC::EOF], token_categories(b"<="));
    assert_eq!(vec![TC::Gt, TC::EOF], token_categories(b">"));
    assert_eq!(vec![TC::Ge, TC::EOF], token_categories(b">="));
    assert_eq!(vec![TC::Comma, TC::EOF], token_categories(b","));
    assert_eq!(vec![TC::Not, TC::EOF], token_categories(b"not"));
    assert_eq!(vec![TC::And, TC::EOF], token_categories(b"and"));
    assert_eq!(vec![TC::Or, TC::EOF], token_categories(b"or"));
    assert_eq!(vec![TC::None, TC::EOF], token_categories(b"none"));
    assert_eq!(vec![TC::One, TC::EOF], token_categories(b"one"));
    assert_eq!(vec![TC::All, TC::EOF], token_categories(b"all"));
    assert_eq!(vec![TC::Of, TC::EOF], token_categories(b"of"));
    assert_eq!(vec![TC::Is, TC::EOF], token_categories(b"is"));
    assert_eq!(vec![TC::Null, TC::EOF], token_categories(b"null"));
    assert_eq!(vec![TC::In, TC::EOF], token_categories(b"in"));
}

#[cfg(test)]
fn token_categories(input: &[u8]) -> Vec<TC> {
    Scanner::scan(input.to_vec()).unwrap_or(vec![]).iter().map(|t| t.cat).collect()
}
