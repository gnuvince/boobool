use errors::{Result, Error};
use tokens::Token;
use tokens::TokenCategory as TC;
use ast::{ExprCategory, UntypedExpr, CmpOp, SetOp};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    offset: usize,
}


fn mkexpr(ec: ExprCategory<()>, pos: usize) -> UntypedExpr {
    return UntypedExpr {
        category: ec,
        pos: pos,
        ty: ()
    };
}


impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<UntypedExpr> {
        let mut parser = Parser { tokens: tokens, offset: 0 };
        return parser.parse_expr();
    }


    fn parse_expr(&mut self) -> Result<UntypedExpr> {
        let expr = self.parse_or()?;
        match self.peek().cat {
            TC::Eof => Ok(expr),
            _ => Err(Error::ExtraTokens(self.offset)),
        }
    }


    fn parse_or(&mut self) -> Result<UntypedExpr> {
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
            let pos = subexprs[0].pos;
            return Ok(mkexpr(ExprCategory::Or(subexprs), pos));
        }
    }


    fn parse_and(&mut self) -> Result<UntypedExpr> {
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
            let pos = subexprs[0].pos;
            return Ok(mkexpr(ExprCategory::And(subexprs), pos));
        }
    }


    fn parse_simple(&mut self) -> Result<UntypedExpr> {
        if self.looking_at(&[TC::Var, TC::LParen]) {
            self.offset -= 1;
            let pos = self.peek_prev().offset;
            let func_name = self.peek_prev().lexeme_string()?;
            let args = self.parse_list_op()?;
            return Ok(mkexpr(ExprCategory::Call(func_name, args), pos));
        }
        if self.looking_at(&[TC::LParen]) {
            let expr = self.parse_or()?;
            if !self.looking_at(&[TC::RParen]) {
                return Err(Error::InvalidToken(self.offset));
            }
            return Ok(expr);
        }
        if self.looking_at(&[TC::Not]) {
            let pos = self.peek_prev().offset;
            let expr = self.parse_simple()?;
            return Ok(mkexpr(ExprCategory::Not(Box::new(expr)), pos));
        }
        if self.looking_at(&[TC::Var, TC::Is, TC::Null]) {
            let t = self.peek_at(self.offset - 3);
            let pos = t.offset;
            let var_name = t.lexeme_string()?;
            return Ok(mkexpr(ExprCategory::IsNull(var_name), pos));
        }
        if self.looking_at(&[TC::Var, TC::Is, TC::Not, TC::Null]) {
            let t = self.peek_at(self.offset - 4);
            let pos = t.offset;
            let var_name = t.lexeme_string()?;
            let is_null = mkexpr(ExprCategory::IsNull(var_name), pos);
            return Ok(mkexpr(ExprCategory::Not(Box::new(is_null)), pos));
        }

        let op1 = self.parse_op()?;
        let pos = op1.pos;
        if self.looking_at(&[TC::In]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::In(Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::Not, TC::In]) {
            let op2 = self.parse_op()?;
            let in_expr = mkexpr(ExprCategory::In(Box::new(op1), Box::new(op2)), pos);
            return Ok(mkexpr(ExprCategory::Not(Box::new(in_expr)), pos));
        }

        if self.looking_at(&[TC::None, TC::Of]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::SetOp(
                SetOp::NoneOf, Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::One, TC::Of]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::SetOp(
                SetOp::OneOf, Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::All, TC::Of]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::SetOp(
                SetOp::AllOf, Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::Eq]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::Compare(
                CmpOp::Eq, Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::Ne]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::Compare(
                CmpOp::Ne, Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::Lt]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::Compare(
                CmpOp::Lt, Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::Le]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::Compare(
                CmpOp::Le, Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::Gt]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::Compare(
                CmpOp::Gt, Box::new(op1), Box::new(op2)), pos));
        }
        if self.looking_at(&[TC::Ge]) {
            let op2 = self.parse_op()?;
            return Ok(mkexpr(ExprCategory::Compare(
                CmpOp::Ge, Box::new(op1), Box::new(op2)), pos));
        }

        return Ok(op1);
    }


    fn parse_op(&mut self) -> Result<UntypedExpr> {
        if self.looking_at(&[TC::Var]) {
            let t = self.peek_prev();
            let pos = t.offset;
            let var_name = t.lexeme_string()?;
            return Ok(mkexpr(ExprCategory::Var(var_name), pos));
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


    fn parse_lit(&mut self) -> Result<UntypedExpr> {
        if self.looking_at(&[TC::IntLiteral]) {
            let t = self.peek_prev();
            let pos = t.offset;
            let val = t.lexeme_i64()?;
            return Ok(mkexpr(ExprCategory::Int(val), pos));
        }
        if self.looking_at(&[TC::FloatLiteral]) {
            let t = self.peek_prev();
            let pos = t.offset;
            let val = t.lexeme_f64()?;
            return Ok(mkexpr(ExprCategory::Float(val), pos));
        }
        if self.looking_at(&[TC::StrLiteral]) {
            let t = self.peek_prev();
            let pos = t.offset;
            let val = t.lexeme_string()?;
            return Ok(mkexpr(ExprCategory::Str(val), pos));
        }
        return Err(Error::InvalidOperator(self.offset));
    }


    fn parse_list_lit(&mut self) -> Result<UntypedExpr> {
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

        return Ok(mkexpr(ExprCategory::List(exprs), initial_offset));
    }


    fn parse_list_op(&mut self) -> Result<Vec<UntypedExpr>> {
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
fn test_list_ops() {
    assert!(test::parse(b"x in xs").is_ok());
    assert!(test::parse(b"x in (1,2,3)").is_ok());
    assert!(test::parse(b"1 in (1,2,3)").is_ok());
    assert!(test::parse(b"1 in xs").is_ok());
    assert!(test::parse(b"x not in xs").is_ok());
    assert!(test::parse(b"x not in (1,2,3)").is_ok());
    assert!(test::parse(b"1 not in (1,2,3)").is_ok());
    assert!(test::parse(b"1 not in xs").is_ok());
    assert!(test::parse(b"1 in (x and y)").is_err());
    assert!(test::parse(b"1 not in (x and y)").is_err());

    assert!(test::parse(b"x none of xs").is_ok());
    assert!(test::parse(b"x none of (1,2,3)").is_ok());
    assert!(test::parse(b"1 none of (1,2,3)").is_ok());
    assert!(test::parse(b"1 none of xs").is_ok());
    assert!(test::parse(b"(1,2) none of xs").is_err());

    assert!(test::parse(b"x one of xs").is_ok());
    assert!(test::parse(b"x one of (1,2,3)").is_ok());
    assert!(test::parse(b"1 one of (1,2,3)").is_ok());
    assert!(test::parse(b"1 one of xs").is_ok());
    assert!(test::parse(b"(1,2) one of xs").is_err());

    assert!(test::parse(b"x all of xs").is_ok());
    assert!(test::parse(b"x all of (1,2,3)").is_ok());
    assert!(test::parse(b"1 all of (1,2,3)").is_ok());
    assert!(test::parse(b"1 all of xs").is_ok());
    assert!(test::parse(b"(1,2) all of xs").is_err());
}

#[test]
fn test_cmp_ops() {
    assert!(test::parse(b"x = y").is_ok());
    assert!(test::parse(b"x <> 1").is_ok());
    assert!(test::parse(b"1 < x").is_ok());
    assert!(test::parse(b"1 <= 2").is_ok());
    assert!(test::parse(b"4.4 > 'foo'").is_ok());
    assert!(test::parse(b"b >= (2,3,5,7)").is_ok());
}


#[test]
fn test_var() {
    assert!(test::parse(b"var").is_ok());
    assert!(test::parse(b"var is null").is_ok());
    assert!(test::parse(b"var is not null").is_ok());

    assert!(test::parse(b"1 is null").is_err());
    assert!(test::parse(b"'foo' is not null").is_err());
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
    use ast::UntypedExpr;
    use super::Parser;

    pub fn parse(input: &[u8]) -> Result<UntypedExpr> {
        let toks = Scanner::scan(input.to_vec())?;
        let x = Parser::parse(toks);
        return x;
    }
}
