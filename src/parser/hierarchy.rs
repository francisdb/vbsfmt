use crate::lexer::{Token, TokenKind};
use crate::parser::ast::{Argument, ErrorClause, FullIdent, Stmt};
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
            T![option] => {
                self.consume(T![option]);
                let explicit = self.next().expect("Expected identifier after `option`");
                match explicit.kind {
                    T![ident] => {
                        assert_eq!(
                            self.text(explicit).to_ascii_lowercase(),
                            "explicit",
                            "Expected `explicit` after `option`"
                        );
                    }
                    _ => panic!("Expected `explicit` after `option`"),
                }
                self.consume_if_not_eof(T![nl]);
                ast::Item::OptionExplicit
            }
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
                let body = self.block(&[T![end]]);

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
                let body = self.block(&[T![end]]);

                self.consume(T![end]);
                self.consume(T![sub]);
                self.consume(T![nl]);

                ast::Item::Sub {
                    name,
                    parameters,
                    body,
                }
            }
            _ => {
                // this must be a statement
                let stmt = self.statement();
                ast::Item::Statement(stmt)
            }
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
    pub fn block(&mut self, end_tokens: &[TokenKind]) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !end_tokens.contains(&self.peek()) {
            if !self.at(T![nl]) || self.at(T![:]) {
                let stmt = self.statement();
                stmts.push(stmt);
            }
            if end_tokens.contains(&self.peek()) {
                break;
            } else if self.at(T![nl]) {
                self.consume(T![nl]);
            } else if self.at(T![:]) {
                self.consume(T![:]);
            }
        }
        stmts
    }

    pub fn statement(&mut self) -> ast::Stmt {
        match self.peek() {
            T![public] => {
                self.consume(T![public]);
                // TODO next could be const, dim, sub, function
                // is this allowed in the root scope?
                unimplemented!("Public not implemented yet")
            }
            T![private] => {
                self.consume(T![public]);
                // TODO next could be const, dim, sub, function
                // is this allowed in the root scope?
                unimplemented!("Public not implemented yet")
            }

            T![dim] => {
                self.consume(T![dim]);
                let mut vars = Vec::new();
                while !self.at(T![nl]) && !self.at(T![EOF]) {
                    let ident = self.next().expect("Expected identifier after `dim`");
                    assert_eq!(
                        ident.kind,
                        T![ident],
                        "Expected identifier after `dim`, but found `{}`",
                        ident.kind
                    );
                    let name = self.text(ident).to_string();

                    let mut dimensions = Vec::new();
                    if self.at(T!['(']) {
                        self.consume(T!['(']);
                        while !self.at(T![')']) {
                            let dimension = self.expression();
                            dimensions.push(dimension);
                            if self.at(T![,]) {
                                self.consume(T![,]);
                            }
                        }
                        self.consume(T![')']);
                    }

                    vars.push((name, dimensions));

                    if self.at(T![,]) {
                        self.consume(T![,]);
                    } else {
                        break;
                    }
                }

                self.consume_line_delimiter();
                Stmt::Dim { vars }
            }
            T![redim] => {
                unimplemented!("ReDim not implemented yet")
            }
            T![const] => {
                // TODO add support for multiple variables in one const statement
                self.consume(T![const]);
                let ident = self.next().expect("Expected identifier after `const`");
                assert_eq!(
                    ident.kind,
                    T![ident],
                    "Expected identifier after `const`, but found `{}`",
                    ident.kind
                );
                let name = self.text(ident).to_string();
                self.consume(T![=]);
                let value = self.expression();
                self.consume_line_delimiter();
                Stmt::Const {
                    var_name: name,
                    value: Box::new(value),
                }
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
                Stmt::Set {
                    var_name: name,
                    value: Box::new(value),
                }
            }
            T![ident] => {
                let ident = self.ident_deep();
                if self.at(T![=]) {
                    // assignment
                    self.consume(T![=]);
                    let value = self.expression();
                    self.consume_line_delimiter();
                    Stmt::Assignment {
                        full_ident: ident,
                        value: Box::new(value),
                    }
                } else if self.at_new_line_or_eof() {
                    // sub call without args
                    self.consume_if_not_eof(T![nl]);
                    Stmt::SubCall {
                        fn_name: ident,
                        args: Vec::new(),
                    }
                } else {
                    // sub call with args
                    let mut args = Vec::new();
                    while !self.at(T![nl]) {
                        let arg = self.expression();
                        args.push(arg);
                        if self.at(T![,]) {
                            self.consume(T![,]);
                        } else {
                            break;
                        }
                    }
                    self.consume_line_delimiter();
                    Stmt::SubCall {
                        fn_name: ident,
                        args,
                    }
                }
            }
            T![if] => {
                self.consume(T![if]);

                let condition = self.expression();

                self.consume(T![then]);

                // if we have a newline, it's a block if statement
                if self.at(T![nl]) {
                    self.consume(T![nl]);
                    let body = self.block(&[T![end], T![else], T![elseif]]);
                    let mut elseif_statements = Vec::new();
                    while self.at(T![elseif]) {
                        self.consume(T![elseif]);
                        let condition = self.expression();
                        self.consume(T![then]);
                        self.consume(T![nl]);
                        let block = self.block(&[T![end], T![else], T![elseif]]);
                        elseif_statements.push((Box::new(condition), block));
                    }
                    let else_stmt = if self.at(T![else]) {
                        self.consume(T![else]);
                        self.consume(T![nl]);
                        Some(self.block(&[T![end]]))
                    } else {
                        None
                    };
                    self.consume(T![end]);
                    self.consume(T![if]);
                    self.consume(T![nl]);
                    Stmt::IfStmt {
                        condition: Box::new(condition),
                        body,
                        elseif_statements,
                        else_stmt,
                    }
                } else {
                    // single line if statement
                    // can contain multiple statements if separated by colons
                    let mut body = Vec::new();
                    while !self.at(T![nl])
                        && !self.at(T![else])
                        && !self.at(T![elseif])
                        && !self.at(T![end])
                        && !self.at(T![EOF])
                    {
                        let stmt = self.statement();
                        body.push(stmt);
                        if self.at(T![:]) {
                            self.consume(T![:]);
                        }
                    }
                    // if we have an else or elseif, we need to parse that as well
                    if self.at(T![else]) {
                        self.consume(T![else]);
                        self.consume(T![nl]);

                        let block = self.block(&[T![end]]);

                        Stmt::IfStmt {
                            condition: Box::new(condition),
                            body,
                            elseif_statements: Vec::new(),
                            else_stmt: Some(block),
                        }
                    } else if self.at(T![elseif]) {
                        self.consume(T![elseif]);
                        let condition = self.expression();
                        self.consume(T![then]);
                        self.consume(T![nl]);

                        let block = self.block(&[T![end]]);

                        Stmt::IfStmt {
                            condition: Box::new(condition),
                            body,
                            elseif_statements: Vec::new(),
                            else_stmt: Some(block),
                        }
                    } else {
                        self.consume_if_not_eof(T![nl]);
                        Stmt::IfStmt {
                            condition: Box::new(condition),
                            body,
                            elseif_statements: Vec::new(),
                            else_stmt: None,
                        }
                    }
                }
            }
            T![while] => {
                self.consume(T![while]);
                let condition = self.expression();
                self.consume(T![nl]);

                let body = self.block(&[T![wend]]);

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

                let body = self.block(&[T![next]]);

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
            T![on] => {
                // error handling
                self.consume(T![on]);
                self.consume(T![error]);
                let error_clause = if self.at(T![resume]) {
                    self.consume(T![resume]);
                    self.consume(T![next]);
                    ErrorClause::ResumeNext
                } else if self.at(T![goto]) {
                    self.consume(T![goto]);
                    let token = self.consume(T![integer_literal]);
                    let number: usize = self
                        .text(token)
                        .parse()
                        .expect("Expected integer after `goto`");
                    if number != 0 {
                        panic!("Expected `goto 0` after `on error`")
                    }
                    ErrorClause::Goto0
                } else {
                    panic!("Expected `resume next` or `goto 0` after `on error`")
                };
                self.consume(T![nl]);
                ast::Stmt::OnError { error_clause }
            }
            kind => {
                panic!("Unexpected token: {:?}", kind);
            }
        }
    }

    pub(crate) fn ident_deep(&mut self) -> FullIdent {
        let ident = self.next().unwrap();
        let name = self.text(ident).to_string();
        let mut property_accesses = vec![];
        while self.at(T![property_access]) {
            let token = self.next().unwrap();
            let property_name = self.text(token).to_string();
            // validate first character and remover the dot
            assert_eq!(
                property_name.chars().next().unwrap(),
                '.',
                "Expected property access to start with a dot, but found `{}`",
                property_name
            );
            property_accesses.push(property_name[1..].to_string());
        }
        FullIdent {
            name,
            property_accesses,
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
