use crate::lexer::{Token, TokenKind};
use crate::parser::ast::{Argument, Stmt};
use crate::parser::{ast, Parser};
use crate::T;

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn file(&mut self) -> Vec<ast::Item> {
        let mut items = Vec::new();
        while !self.at(T![EOF]) {
            let item = self.item();
            items.push(item);
        }
        items
    }

    pub fn item(&mut self) -> ast::Item {
        match self.peek() {
            T![function] => {
                self.consume(T![function]);

                let ident = self
                    .next()
                    .expect("Tried to parse function name, but there were no more tokens");
                assert_eq!(
                    ident.kind,
                    T![ident],
                    "Expected identifier as function name, but found `{}`",
                    ident.kind
                );
                let name = self.text(ident).to_string();

                let parameters = self.declaration_parameter_list("Function");

                self.consume(T![nl]);
                // do we need to do something special with the returned value?
                let body = self.block(T![end]);

                self.consume(T![end]);
                self.consume(T![function]);
                self.consume(T![nl]);

                ast::Item::Function {
                    name,
                    parameters,
                    body,
                }
            }
            T![sub] => {
                self.consume(T![sub]);

                let ident = self
                    .next()
                    .expect("Tried to parse sub name, but there were no more tokens");
                assert_eq!(
                    ident.kind,
                    T![ident],
                    "Expected identifier as sub name, but found `{}`",
                    ident.kind
                );
                let name = self.text(ident).to_string();

                let parameters = self.declaration_parameter_list("Sub");

                self.consume(T![nl]);
                let body = self.block(T![end]);

                self.consume(T![end]);
                self.consume(T![sub]);
                self.consume(T![nl]);

                ast::Item::Sub {
                    name,
                    parameters,
                    body,
                }
            }
            // T![struct] => {
            //     self.consume(T![struct]);
            //     let mut members = Vec::new();
            //     let name = self.type_();
            //     self.consume(T!['{']);
            //     while !self.at(T!['}']) {
            //         let member_ident = self
            //             .next()
            //             .expect("Tried to parse struct member, but there were no more tokens");
            //         assert_eq!(
            //             member_ident.kind,
            //             T![ident],
            //             "Expected identifier as struct member, but found `{}`",
            //             member_ident.kind
            //         );
            //         let member_name = self.text(member_ident).to_string();
            //         self.consume(T![:]);
            //         let member_type = self.type_();
            //         members.push((member_name, member_type));
            //         if self.at(T![,]) {
            //             self.consume(T![,]);
            //         }
            //     }
            //     self.consume(T!['}']);
            //     ast::Item::Struct { name, members }
            // }
            kind => panic!("Unknown start of item: `{}`", kind),
        }
    }

    /// Parse a list of parameters for a function or sub declaration.
    fn declaration_parameter_list(&mut self, item_type: &str) -> Vec<Argument> {
        let mut parameters: Vec<Argument> = Vec::new();
        self.consume(T!['(']);
        while !self.at(T![')']) {
            // optional modifier
            let modifier = if self.at(T![byval]) {
                self.consume(T![byval]);
                Argument::ByVal
            } else if self.at(T![byref]) {
                self.consume(T![byref]);
                Argument::ByRef
            } else {
                Argument::ByVal
            };
            let parameter_ident = self.next().unwrap_or_else(|| {
                panic!(
                    "Tried to parse {} parameter, but there were no more tokens",
                    item_type
                )
            });
            assert_eq!(
                parameter_ident.kind,
                T![ident],
                "Expected identifier as {} parameter, but found `{}`",
                item_type,
                parameter_ident.kind
            );
            let parameter_name = self.text(parameter_ident).to_string();
            parameters.push(modifier(parameter_name));
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);
        parameters
    }

    /// Parse a block of statements until we reach an `end` token.
    pub fn block(&mut self, end_token: TokenKind) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.at(end_token) {
            println!("Unexpected token: {:?}", self.peek());
            if !self.at(T![nl]) {
                let stmt = self.statement();
                stmts.push(stmt);
            }
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
                if self.at(T![=]) {
                    self.consume(T![=]);
                    let value = self.expression();
                    self.consume(T![nl]);
                    ast::Stmt::Assignment {
                        var_name: name,
                        value: Box::new(value),
                    }
                } else {
                    // function call or sub call
                    // let mut args = Vec::new();
                    // self.consume(T!['(']);
                    // while !self.at(T![')']) {
                    //     let arg = self.expression();
                    //     args.push(arg);
                    //     if self.at(T![,]) {
                    //         self.consume(T![,]);
                    //     }
                    // }
                    // self.consume(T![')']);
                    // self.consume(T![nl]);
                    // ast::Expr::FunctionCall {
                    //     fn_name: name,
                    //     args,
                    // }
                    unimplemented!("Function and sub calls not implemented yet")
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

    pub fn type_(&mut self) -> ast::Type {
        let ident = self
            .next()
            .expect("Tried to parse type, but there were no more tokens");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier at start of type, but found `{}`",
            ident.kind
        );
        let name = self.text(ident).to_string();

        let mut generics = Vec::new();

        if self.at(T![<]) {
            self.consume(T![<]);
            while !self.at(T![>]) {
                // Generic parameters are also types
                let generic = self.type_();
                generics.push(generic);
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![>]);
        }

        ast::Type { name, generics }
    }
}
