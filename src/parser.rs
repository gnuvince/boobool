use errors::{Result, Error};
use tokens::Token;
use tokens::TokenCategory as TC;
use untyped_ast::{Expr, CmpOp};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    offset: usize,
}


impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Expr> {
        let mut parser = Parser { tokens: tokens, offset: 0 };
        return parser.parse_expr();
    }


    fn parse_expr(&mut self) -> Result<Expr> {
        let expr = self.parse_or()?;
        match self.peek().cat {
            TC::Eof => Ok(expr),
            _ => Err(Error::ExtraTokens(self.offset)),
        }
    }


    fn parse_or(&mut self) -> Result<Expr> {
        let mut subexprs = Vec::new();
        subexprs.push(self.parse_and()?);
        while !self.eof() && self.peek().cat == TC::Or {
            self.offset += 1; // eat 'or' keyword
            subexprs.push(self.parse_and()?);
        }
        if subexprs.len() == 1 {
            let expr = subexprs.into_iter().next().unwrap();
            return Ok(expr);
        } else {
            return Ok(Expr::Or(subexprs));
        }
    }


    fn parse_and(&mut self) -> Result<Expr> {
        let mut subexprs = Vec::new();
        subexprs.push(self.parse_simple()?);
        while !self.eof() && self.peek().cat == TC::And {
            self.offset += 1; // eat 'and' keyword
            subexprs.push(self.parse_simple()?);
        }
        if subexprs.len() == 1 {
            let expr = subexprs.into_iter().next().unwrap();
            return Ok(expr);
        } else {
            return Ok(Expr::And(subexprs));
        }
    }


    fn parse_simple(&mut self) -> Result<Expr> {
        if self.looking_at(&[TC::Var, TC::LParen]) {
            self.offset -= 1;
            let func_name = self.peek_prev().lexeme_string()?;
            let args = self.parse_list_op()?;
            return Ok(Expr::Call(func_name, args));
        }
        if self.looking_at(&[TC::LParen]) {
            let expr = self.parse_or()?;
            if !self.looking_at(&[TC::RParen]) {
                return Err(Error::InvalidToken(self.offset));
            }
            return Ok(expr);
        }
        if self.looking_at(&[TC::Not]) {
            let expr = self.parse_simple()?;
            return Ok(Expr::Not(Box::new(expr)));
        }
        if self.looking_at(&[TC::Var, TC::Is, TC::Null]) {
            let var_name = self.peek_at(self.offset - 3).lexeme_string()?;
            return Ok(Expr::IsNull(var_name));
        }
        if self.looking_at(&[TC::Var, TC::Is, TC::Not, TC::Null]) {
            let var_name = self.peek_at(self.offset - 4).lexeme_string()?;
            return Ok(Expr::Not(Box::new(Expr::IsNull(var_name))));
        }

        let op1 = self.parse_op()?;
        if self.looking_at(&[TC::In]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::In(Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::Not, TC::In]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::Not(Box::new(Expr::In(Box::new(op1), Box::new(op2)))));
        }

        if self.looking_at(&[TC::None, TC::Of]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::NoneOf(Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::One, TC::Of]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::OneOf(Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::All, TC::Of]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::AllOf(Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::Eq]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::Compare(CmpOp::Eq, Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::Ne]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::Compare(CmpOp::Ne, Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::Lt]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::Compare(CmpOp::Lt, Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::Le]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::Compare(CmpOp::Le, Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::Gt]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::Compare(CmpOp::Gt, Box::new(op1), Box::new(op2)));
        }
        if self.looking_at(&[TC::Ge]) {
            let op2 = self.parse_op()?;
            return Ok(Expr::Compare(CmpOp::Ge, Box::new(op1), Box::new(op2)));
        }

        return Ok(op1);
    }


    fn parse_op(&mut self) -> Result<Expr> {
        if self.looking_at(&[TC::Var]) {
            let var_name = self.peek_prev().lexeme_string()?;
            return Ok(Expr::Var(var_name));
        }
        if self.looking_at(&[TC::IntLiteral]) ||
            self.looking_at(&[TC::FloatLiteral]) ||
            self.looking_at(&[TC::StrLiteral]) {
                self.offset -= 1;
                return self.parse_lit();
        }
        if self.looking_at(&[TC::LParen]) {
            self.offset -= 1;
            return self.parse_list_lit();
        }
        return Err(Error::InvalidOperator(self.offset));
    }


    fn parse_lit(&mut self) -> Result<Expr> {
        if self.looking_at(&[TC::IntLiteral]) {
            let val = self.peek_prev().lexeme_i64()?;
            return Ok(Expr::Int(val));
        }
        if self.looking_at(&[TC::FloatLiteral]) {
            let val = self.peek_prev().lexeme_f64()?;
            return Ok(Expr::Float(val));
        }
        if self.looking_at(&[TC::StrLiteral]) {
            let val = self.peek_prev().lexeme_string()?;
            return Ok(Expr::Str(val));
        }
        return Err(Error::InvalidOperator(self.offset));
    }


    fn parse_list_lit(&mut self) -> Result<Expr> {
        let initial_offset = self.offset;
        let mut exprs = Vec::new();

        if !self.looking_at(&[TC::LParen]) {
            return Err(Error::InvalidToken(self.offset));
        }

        exprs.push(self.parse_lit()?);
        while !self.eof() && self.peek().cat != TC::RParen {
            if !self.looking_at(&[TC::Comma]) {
                return Err(Error::InvalidToken(self.offset));
            }
            exprs.push(self.parse_lit()?);
        }

        if self.eof() {
            return Err(Error::UnterminatedList(initial_offset));
        }

        if !self.looking_at(&[TC::RParen]) {
            return Err(Error::InvalidToken(self.offset));
        }

        return Ok(Expr::List(exprs));
    }


    fn parse_list_op(&mut self) -> Result<Vec<Expr>> {
        let initial_offset = self.offset;
        let mut exprs = Vec::new();

        if !self.looking_at(&[TC::LParen]) {
            return Err(Error::InvalidToken(self.offset));
        }

        exprs.push(self.parse_op()?);
        while !self.eof() && self.peek().cat != TC::RParen {
            if !self.looking_at(&[TC::Comma]) {
                return Err(Error::InvalidToken(self.offset));
            }
            exprs.push(self.parse_op()?);
        }

        if self.eof() {
            return Err(Error::UnterminatedList(initial_offset));
        }

        if !self.looking_at(&[TC::RParen]) {
            return Err(Error::InvalidToken(self.offset));
        }

        return Ok(exprs);
    }


    fn peek(&self) -> &Token {
        return self.peek_at(self.offset);
    }


    fn peek_prev(&self) -> &Token {
        return self.peek_at(self.offset - 1);
    }

    fn looking_at(&mut self, cats: &[TC]) -> bool {
        for (i, cat1) in cats.iter().enumerate() {
            let cat2 = self.peek_at(self.offset + i).cat;
            if *cat1 != cat2 {
                return false;
            }
        }
        self.offset += cats.len();
        return true;
    }


    fn peek_at(&self, offset: usize) -> &Token {
        if offset >= self.tokens.len() {
            return &self.tokens[self.tokens.len() - 1];
        } else {
            return &self.tokens[offset];
        }
    }


    fn eof(&self) -> bool {
        let t = self.peek();
        return t.cat == TC::Eof;
    }
}



#[test]
fn test_and_or() {
    assert!(test::parse(b"x").is_ok());
    assert!(test::parse(b"x and y").is_ok());
    assert!(test::parse(b"x and y and z").is_ok());
    assert!(test::parse(b"x or y").is_ok());
    assert!(test::parse(b"x or y or z").is_ok());
    assert!(test::parse(b"x or y and z").is_ok());
    assert!(test::parse(b"x and y or z").is_ok());
    assert!(test::parse(b"x and").is_err());
    assert!(test::parse(b"x or").is_err());
}

#[test]
fn test_int() {
    assert!(test::parse(b"14").is_ok());
}

#[test]
fn test_float() {
    assert!(test::parse(b"1.4").is_ok());
    assert!(test::parse(b".4").is_err());
    assert!(test::parse(b"1.").is_err());
}

#[test]
fn test_str() {
    assert!(test::parse(b"\"\"").is_ok());
    assert!(test::parse(b"\"foo\"").is_ok());
    assert!(test::parse(b"\"'quotes'\"").is_ok());
    assert!(test::parse(b"''").is_ok());
    assert!(test::parse(b"'\"quotes\"'").is_ok());
    assert!(test::parse(b"'foo'").is_ok());
}

#[test]
fn test_list() {
    assert!(test::parse(b"x in (1)").is_ok());
    assert!(test::parse(b"x in (1, 2, 3)").is_ok());
    assert!(test::parse(b"x in (1.0, 2.0, 3.0)").is_ok());
    assert!(test::parse(b"x in ('one', 'two', 'three')").is_ok());
    assert!(test::parse(b"x in (1 2 3)").is_err());
    assert!(test::parse(b"x in (1").is_err());
    assert!(test::parse(b"x in 1)").is_err());
    assert!(test::parse(b"x in (a, b, c)").is_err());
    assert!(test::parse(b"x in (3 = 4)").is_err());
    assert!(test::parse(b"x in (3 and 4)").is_err());
    assert!(test::parse(b"x in ()").is_err());
}

#[test]
fn test_parens() {
    assert!(test::parse(b"(x)").is_ok());
    assert!(test::parse(b"(x and y)").is_ok());
    assert!(test::parse(b"(x or y)").is_ok());
    assert!(test::parse(b"(((x)) or (y))").is_ok());
    assert!(test::parse(b"(x and y").is_err());
    assert!(test::parse(b"x and y)").is_err());
}


#[test]
fn test_var() {
    assert!(test::parse(b"var").is_ok());
}

#[test]
fn test_call() {
    assert!(test::parse(b"f(x)").is_ok());
    assert!(test::parse(b"f(x, y)").is_ok());
    assert!(test::parse(b"f(x, 1, 2.3, 'hello', (1,2,3))").is_ok());
    assert!(test::parse(b"f()").is_err());
}



#[cfg(test)]
mod test {
    use scanner::Scanner;
    use errors::Result;
    use untyped_ast::Expr;
    use super::Parser;

    pub fn parse(input: &[u8]) -> Result<Expr> {
        let toks = Scanner::scan(input.to_vec())?;
        let x = Parser::parse(toks);
        return x;
    }
}
