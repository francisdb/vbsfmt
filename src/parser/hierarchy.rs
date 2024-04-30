use crate::lexer::{Token, TokenKind};
use crate::parser::ast::Stmt;
use crate::parser::{ast, Parser};
use crate::T;

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    /// Parse a block of statements until we reach an `end` token.
    pub fn block(&mut self, end_token: TokenKind) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.at(end_token) {
            let stmt = self.statement();
            stmts.push(stmt);
            if self.at(end_token) {
                break;
            }
            self.consume(T![nl]);
        }
        stmts
    }

    pub fn statement(&mut self) -> ast::Stmt {
        match self.peek() {
            // TODO add options
            T![nl] => {
                // skip empty lines
                self.consume(T![nl]);
                self.statement()
            }
            T![dim] => {
                unimplemented!("Dim not implemented yet")
            }
            T![set] => {
                self.consume(T![set]);
                let ident = self.next().expect("Expected identifier after `let`");
                assert_eq!(
                    ident.kind,
                    T![ident],
                    "Expected identifier after `let`, but found `{}`",
                    ident.kind
                );
                let name = self.text(ident).to_string();
                self.consume(T![=]);
                let value = self.expression();
                self.consume(T![nl]);
                ast::Stmt::Set {
                    var_name: name,
                    value: Box::new(value),
                }
            }
            T![ident] => {
                let ident = self.next().unwrap();
                let name = self.text(ident).to_string();
                self.consume(T![=]);
                let value = self.expression();
                self.consume(T![nl]);
                ast::Stmt::Assignment {
                    var_name: name,
                    value: Box::new(value),
                }
            }
            T![if] => {
                self.consume(T![if]);

                let condition = self.expression();

                self.consume(T![then]);

                // TODO handle (multiple) else and elseif

                let body = self.block(T![end]);

                let else_stmt = None;
                // let else_stmt = if self.at(T![else]) {
                //     self.consume(T![else]);
                //     assert!(
                //         self.at(T![if]) || self.at(T!['{']),
                //         "Expected a block or an `if` after `else` statement"
                //     );
                //     Some(Box::new(self.statement()))
                // } else {
                //     None
                // };

                self.consume(T![end]);
                self.consume(T![if]);
                self.consume(T![nl]);

                ast::Stmt::IfStmt {
                    condition: Box::new(condition),
                    body,
                    else_stmt,
                }
            }
            T![while] => {
                self.consume(T![while]);
                let condition = self.expression();
                self.consume(T![nl]);

                let body = self.block(T![wend]);

                self.consume(T![wend]);
                self.consume(T![nl]);

                ast::Stmt::WhileStmt {
                    condition: Box::new(condition),
                    body,
                }
            }
            T![for] => {
                self.consume(T![for]);
                let counter = self.next().unwrap();
                let counter_name = self.text(counter).to_string();
                self.consume(T![=]);
                let start = self.expression();
                self.consume(T![to]);
                let end = self.expression();
                let step = if self.at(T![step]) {
                    self.consume(T![step]);
                    Some(Box::new(self.expression()))
                } else {
                    None
                };
                self.consume(T![nl]);

                let body = self.block(T![next]);

                self.consume(T![next]);
                self.consume(T![nl]);

                ast::Stmt::ForStmt {
                    counter: counter_name,
                    start: Box::new(start),
                    end: Box::new(end),
                    step,
                    body,
                }
            }
            kind => {
                panic!("Unexpected token: {:?}", kind);
            }
        }
    }
}
