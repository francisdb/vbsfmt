use crate::lexer::{Token, TokenKind};
use crate::parser::ast::{
    Argument, ArgumentType, ErrorClause, Expr, FullIdent, IdentPart, Item, MemberAccess,
    MemberDefinitions, PropertyType, PropertyVisibility, SetRhs, Stmt, VarRef, Visibility,
};
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
            T![option] => self.item_option(),
            T![function] => self.item_function(),
            T![sub] => self.item_sub(),
            T![class] => self.item_class(),
            _ => {
                // this must be a statement
                let stmt = self.statement(true);
                Item::Statement(stmt)
            }
        }
    }

    fn item_class(&mut self) -> Item {
        self.consume(T![class]);
        let ident = self.consume(T![ident]);
        let name = self.text(&ident).to_string();
        self.consume_line_delimiter();
        let mut members = Vec::new();
        let mut member_accessors = Vec::new();
        let mut methods = Vec::new();
        let mut dims = Vec::new();
        while !self.at(T![end]) {
            let mut default = None;
            let visibility = if self.at(T![public]) {
                self.consume(T![public]);
                if self.at(T![default]) {
                    self.consume(T![default]);
                    default = Some(true);
                }
                Some(Visibility::Public)
            } else if self.at(T![private]) {
                self.consume(T![private]);
                Some(Visibility::Private)
            } else {
                None
            };

            match self.peek() {
                T![property] => member_accessors.push(self.class_property(default, visibility)),
                T![function] => methods.push(self.class_function(visibility)),
                T![sub] => methods.push(self.class_sub(visibility)),
                T![dim] => {
                    // visibility can not be set for dims
                    if visibility.is_some() {
                        let peek = self.peek_full();
                        panic!(
                            "Class dim does not support visibility at line {}, column {} but found '{}'",
                            peek.line, peek.column, peek.kind
                        )
                    }
                    dims.push(self.class_dim());
                }
                _ => {
                    members.push(self.class_member(visibility));
                }
            }
        }
        self.consume(T![end]);
        self.consume(T![class]);
        self.consume_if_not_eof(T![nl]);
        Item::Class {
            name,
            members,
            dims,
            member_accessors,
            methods,
        }
    }

    fn class_member(&mut self, visibility: Option<Visibility>) -> MemberDefinitions {
        // properties
        let visibility = visibility.unwrap_or_else(|| {
            let peek = self.peek_full();
            panic!(
                "Expected visibility for class member at line {}, column {} but found '{}'",
                peek.line, peek.column, peek.kind
            )
        });
        // like a dim we can have multiple properties in one line of which some can be arrays
        let mut properties = Vec::new();
        while {
            let ident = self.consume(T![ident]);
            let name = self.text(&ident).to_string();
            let bounds = self.const_bounds();
            properties.push((name, bounds));
            self.at(T![,])
        } {
            self.consume(T![,]);
        }
        let member_definitions = MemberDefinitions {
            visibility,
            properties,
        };
        self.consume_line_delimiter();
        member_definitions
    }

    fn class_dim(&mut self) -> Vec<(String, Vec<usize>)> {
        self.consume(T![dim]);
        let mut vars = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let ident = self.consume(T![ident]);
            let name = self.text(&ident).to_string();
            let bounds = self.const_bounds();
            vars.push((name, bounds));
            if self.at(T![,]) {
                self.consume(T![,]);
            } else {
                break;
            }
        }
        self.consume_line_delimiter();
        vars
    }

    fn class_sub(&mut self, visibility: Option<Visibility>) -> (Visibility, Item) {
        self.consume(T![sub]);
        let ident = self.consume(T![ident]);
        let method_name = self.text(&ident).to_string();
        let parameters = self.optional_declaration_parameter_list("Sub");
        self.consume_line_delimiter();
        let body = self.block(&[T![end]]);
        self.consume(T![end]);
        self.consume(T![sub]);
        self.consume_if_not_eof(T![nl]);
        let item = ast::Item::Sub {
            name: method_name.clone(),
            parameters,
            body,
        };
        let sub_visibility = visibility.unwrap_or(Visibility::Public);
        (sub_visibility, item)
    }

    fn class_function(&mut self, visibility: Option<Visibility>) -> (Visibility, Item) {
        self.consume(T![function]);
        let ident = self.consume(T![ident]);
        let method_name = self.text(&ident).to_string();
        let parameters = self.optional_declaration_parameter_list("Function");
        self.consume_line_delimiter();
        let body = self.block(&[T![end]]);
        self.consume(T![end]);
        self.consume(T![function]);
        self.consume_if_not_eof(T![nl]);
        let item = ast::Item::Function {
            name: method_name.clone(),
            parameters,
            body,
        };
        let function_visibility = visibility.unwrap_or(Visibility::Public);
        (function_visibility, item)
    }

    fn class_property(
        &mut self,
        default: Option<bool>,
        visibility: Option<Visibility>,
    ) -> MemberAccess {
        let property_visibility = match visibility {
            Some(Visibility::Public) => PropertyVisibility::Public {
                default: default.unwrap_or(false),
            },
            Some(Visibility::Private) => PropertyVisibility::Private,
            None => PropertyVisibility::Public {
                default: default.unwrap_or(false),
            },
        };

        self.consume(T![property]);
        // let (Variant), get or set (Object)
        let property_type = match self.peek() {
            T![let] => {
                self.consume(T![let]);
                PropertyType::Let
            }
            T![set] => {
                self.consume(T![set]);
                PropertyType::Set
            }
            T![get] => {
                self.consume(T![get]);
                PropertyType::Get
            }
            other => {
                let peek = self.peek_full();
                panic!(
                    "Expected `let`, `set` or `get` in class property definition at line {}, column {}, got `{}`",
                    peek.line, peek.column, other
                );
            }
        };

        let ident = self.consume(T![ident]);
        let name = self.text(&ident).to_string();
        let property_arguments = self.optional_parenthesized_property_arguments();

        let property_body = self.block(&[T![end]]);
        self.consume(T![end]);
        self.consume(T![property]);
        self.consume_line_delimiter();
        MemberAccess {
            visibility: property_visibility,
            name,
            property_type,
            args: property_arguments,
            body: property_body,
        }
    }

    fn item_option(&mut self) -> Item {
        self.consume(T![option]);
        let explicit = self.next().expect("Expected identifier after `option`");
        match explicit.kind {
            T![ident] => {
                assert_eq!(
                    self.text(&explicit).to_ascii_lowercase(),
                    "explicit",
                    "Expected `explicit` after `option`"
                );
            }
            _ => panic!("Expected `explicit` after `option`"),
        }
        self.consume_if_not_eof(T![nl]);
        Item::OptionExplicit
    }

    fn item_sub(&mut self) -> Item {
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
        let name = self.text(&ident).to_string();
        let parameters = self.optional_declaration_parameter_list("Sub");
        self.consume_line_delimiter();
        let body = self.block(&[T![end]]);

        self.consume(T![end]);
        self.consume(T![sub]);
        self.consume_if_not_eof(T![nl]);

        Item::Sub {
            name,
            parameters,
            body,
        }
    }

    fn item_function(&mut self) -> Item {
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
        let name = self.text(&ident).to_string();

        let parameters = self.optional_declaration_parameter_list("Function");

        self.consume(T![nl]);
        // do we need to do something special with the returned value?
        let body = self.block(&[T![end]]);

        self.consume(T![end]);
        self.consume(T![function]);
        self.consume(T![nl]);

        Item::Function {
            name,
            parameters,
            body,
        }
    }

    fn const_bounds(&mut self) -> Vec<usize> {
        let mut bounds = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                let dim = self.consume(T![integer_literal]);
                let dim: usize = match self.text(&dim).parse() {
                    Ok(dim) => dim,
                    Err(_) => panic!(
                        "Expected integer literal as bound at line {}, row {}",
                        dim.line, dim.column
                    ),
                };
                bounds.push(dim);
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        }
        bounds
    }

    /// Parse a list of parameters for a function or sub declaration.
    fn optional_declaration_parameter_list(&mut self, item_type: &str) -> Vec<Argument> {
        let mut parameters: Vec<Argument> = Vec::new();
        if self.at(T!['(']) {
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
                let parameter_name = self.text(&parameter_ident).to_string();
                parameters.push(modifier(parameter_name));
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        }
        parameters
    }

    /// Parse a block of statements until we reach an `end` token.
    pub fn block(&mut self, end_tokens: &[TokenKind]) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !end_tokens.contains(&self.peek()) {
            if !self.at(T![nl]) && !self.at(T![:]) {
                let stmt = self.statement(false);
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

    pub fn statement(&mut self, consume_delimiter: bool) -> Stmt {
        let stmt = match self.peek() {
            // T![public] => {
            //     self.consume(T![public]);
            //     // TODO next could be const, dim, sub, function
            //     // is this allowed in the root scope?
            //     unimplemented!("Public not implemented yet")
            // }
            // T![private] => {
            //     self.consume(T![public]);
            //     // TODO next could be const, dim, sub, function
            //     // is this allowed in the root scope?
            //     unimplemented!("Public not implemented yet")
            // }
            T![dim] => self.statement_dim(),
            T![redim] => self.statement_redim(),
            T![const] => self.statement_const(),
            T![set] => self.statement_set(),
            T![if] => self.statement_if(),
            T![while] => self.statement_while(),
            T![for] => self.statement_for(),
            T![select] => self.statement_select(),
            T![on] => self.statement_on(),
            T![exit] => self.statement_exit(),
            T![with] => self.statement_with(),
            T![ident] | T![me] | T![property_access] => {
                // TODO we don't want to allow property access in case we are not in a with block
                let ident = self.ident_deep();
                if self.at(T![=]) {
                    // assignment
                    self.consume(T![=]);
                    let value = self.expression();
                    Stmt::Assignment {
                        full_ident: ident,
                        value: Box::new(value),
                    }
                } else if self.at_new_line_or_eof() {
                    // sub call without args
                    Stmt::SubCall {
                        fn_name: ident,
                        args: Vec::new(),
                    }
                } else {
                    // sub call with args
                    let mut args = Vec::new();
                    while !self.at(T![:]) && !self.at(T![nl]) && !self.at(T![EOF]) {
                        // // empty arguments are allowed
                        if self.at(T![,]) {
                            self.consume(T![,]);
                            args.push(None);
                            continue;
                        }
                        let arg = self.expression();
                        args.push(Some(arg));
                        if self.at(T![,]) {
                            self.consume(T![,]);
                        } else {
                            break;
                        }
                    }
                    Stmt::SubCall {
                        fn_name: ident,
                        args,
                    }
                }
            }
            unexpected => {
                let full = self.peek_full();
                panic!(
                    "Unexpected token: {:?} at line {}, column {}",
                    unexpected, full.line, full.column
                );
            }
        };
        if consume_delimiter {
            self.consume_line_delimiter();
        }
        stmt
    }

    fn statement_dim(&mut self) -> Stmt {
        self.consume(T![dim]);
        let mut vars = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let ident = self.consume(T![ident]);
            let name = self.text(&ident).to_string();
            let bounds = self.parenthesized_arguments();
            vars.push((name, bounds));
            if self.at(T![,]) {
                self.consume(T![,]);
            } else {
                break;
            }
        }
        Stmt::Dim { vars }
    }

    fn statement_redim(&mut self) -> Stmt {
        self.consume(T![redim]);
        let mut preserve = false;
        if self.at(T![preserve]) {
            self.consume(T![preserve]);
            preserve = true;
        }
        let ident = self.consume(T![ident]);
        let var_name = self.text(&ident).to_string();
        let bounds = self.parenthesized_arguments();
        Stmt::ReDim {
            var_name,
            preserve,
            bounds,
        }
    }

    fn statement_const(&mut self) -> Stmt {
        // TODO add support for multiple variables in one const statement
        self.consume(T![const]);
        let ident = self.consume(T![ident]);
        let name = self.text(&ident).to_string();
        self.consume(T![=]);
        let value = self.expression();
        Stmt::Const {
            var_name: name,
            value: Box::new(value),
        }
    }

    fn statement_with(&mut self) -> Stmt {
        self.consume(T![with]);
        let object = self.ident_deep();
        self.consume_line_delimiter();
        let body = self.block(&[T![end]]);
        self.consume(T![end]);
        self.consume(T![with]);
        Stmt::With { object, body }
    }

    fn statement_exit(&mut self) -> Stmt {
        self.consume(T![exit]);
        match self.peek() {
            T![do] => {
                self.consume(T![do]);
                Stmt::ExitDo
            }
            T![for] => {
                self.consume(T![for]);
                Stmt::ExitFor
            }
            T![function] => {
                self.consume(T![function]);
                Stmt::ExitFunction
            }
            T![property] => {
                self.consume(T![property]);
                Stmt::ExitProperty
            }
            T![sub] => {
                self.consume(T![sub]);
                Stmt::ExitSub
            }
            other => panic!("Exit not supported for {}", other),
        }
    }

    fn statement_on(&mut self) -> Stmt {
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
                .text(&token)
                .parse()
                .expect("Expected integer after `goto`");
            if number != 0 {
                panic!("Expected `goto 0` after `on error`")
            }
            ErrorClause::Goto0
        } else {
            panic!("Expected `resume next` or `goto 0` after `on error`")
        };
        Stmt::OnError { error_clause }
    }

    fn statement_select(&mut self) -> Stmt {
        self.consume(T![select]);
        self.consume(T![case]);
        let expr = self.expression();
        self.consume(T![nl]);
        let mut cases = Vec::new();
        let mut else_stmt = None;
        while !self.at(T![end]) {
            if else_stmt.is_some() {
                panic!("`else` statement must be last in `select case` block")
            }
            self.consume(T![case]);
            if self.at(T![else]) {
                self.consume(T![else]);
                self.consume_line_delimiter();
                let block = self.block(&[T![end], T![case]]);
                else_stmt = Some(block);
            } else {
                let mut case_values = Vec::new();
                while !self.at(T![nl]) && !self.at(T![:]) {
                    let value = self.expression();
                    case_values.push(value);
                    if self.at(T![,]) {
                        self.consume(T![,]);
                    }
                }
                self.consume_line_delimiter();
                let block = self.block(&[T![end], T![case]]);
                cases.push((case_values, block));
            }
        }
        self.consume(T![end]);
        self.consume(T![select]);
        Stmt::SelectCase {
            test_expr: Box::new(expr),
            cases,
            else_stmt,
        }
    }

    fn statement_for(&mut self) -> Stmt {
        self.consume(T![for]);

        if self.at(T![each]) {
            self.consume(T![each]);
            let element = self.next().unwrap();
            let element_name = self.text(&element).to_string();
            self.consume(T![in]);
            let group = Box::new(self.expression());
            self.consume_line_delimiter();

            let body = self.block(&[T![next]]);

            self.consume(T![next]);

            Stmt::ForEachStmt {
                element: element_name,
                group,
                body,
            }
        } else {
            let counter = self.next().unwrap();
            let counter_name = self.text(&counter).to_string();
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
            self.consume_line_delimiter();

            let body = self.block(&[T![next]]);

            self.consume(T![next]);

            Stmt::ForStmt {
                counter: counter_name,
                start: Box::new(start),
                end: Box::new(end),
                step,
                body,
            }
        }
    }

    fn statement_while(&mut self) -> Stmt {
        self.consume(T![while]);
        let condition = self.expression();
        self.consume(T![nl]);

        let body = self.block(&[T![wend]]);

        self.consume(T![wend]);

        Stmt::WhileStmt {
            condition: Box::new(condition),
            body,
        }
    }

    fn statement_set(&mut self) -> Stmt {
        self.consume(T![set]);
        let ident = self.consume(T![ident]);
        let name = self.text(&ident).to_string();
        let array_indices = self.parenthesized_arguments();
        let var = VarRef {
            name,
            array_indices,
        };
        self.consume(T![=]);
        let rhs = match self.peek() {
            T![nothing] => {
                self.consume(T![nothing]);
                SetRhs::Nothing
            }
            T![new] => {
                self.consume(T![new]);
                let ident = self.consume(T![ident]);
                let class_name = self.text(&ident).to_string();
                SetRhs::NewClass(class_name)
            }
            _ => {
                let expr = self.expression();
                SetRhs::Expr(Box::new(expr))
            }
        };
        Stmt::Set { var, rhs }
    }

    fn statement_if(&mut self) -> Stmt {
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
                let inline_stmt = self.statement(false);
                body.push(inline_stmt);
                if self.at(T![:]) {
                    self.consume(T![:]);
                }
            }

            let mut elseif_statements = Vec::new();
            while self.at(T![elseif]) {
                self.consume(T![elseif]);
                let condition = self.expression();
                self.consume(T![then]);
                let mut elseif_body = Vec::new();
                while !self.at(T![nl])
                    && !self.at(T![else])
                    && !self.at(T![elseif])
                    && !self.at(T![end])
                    && !self.at(T![EOF])
                {
                    let inline_stmt = self.statement(false);
                    elseif_body.push(inline_stmt);
                    if self.at(T![:]) {
                        self.consume(T![:]);
                    }
                }
                elseif_statements.push((Box::new(condition), elseif_body));
            }
            let else_stmt = if self.at(T![else]) {
                self.consume(T![else]);
                let mut else_body = Vec::new();
                while !self.at(T![end]) && !self.at(T![EOF]) && !self.at(T![nl]) {
                    let inline_stmt = self.statement(false);
                    else_body.push(inline_stmt);
                    if self.at(T![:]) {
                        self.consume(T![:]);
                    }
                }
                Some(else_body)
            } else {
                None
            };

            // optional "End If" if we still have not encountered a newline
            if self.at(T![end]) {
                self.consume(T![end]);
                self.consume(T![if]);
            }

            Stmt::IfStmt {
                condition: Box::new(condition),
                body,
                elseif_statements,
                else_stmt,
            }
        }
    }

    fn parenthesized_arguments(&mut self) -> Vec<Expr> {
        let mut arguments = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                let expr = self.expression();
                arguments.push(expr);
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        };
        arguments
    }

    fn optional_parenthesized_property_arguments(&mut self) -> Vec<(String, ArgumentType)> {
        let mut property_arguments = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                // modifiers for the property
                let argument_type = if self.at(T![byval]) {
                    self.consume(T![byval]);
                    ArgumentType::ByVal
                } else if self.at(T![byref]) {
                    self.consume(T![byref]);
                    ArgumentType::ByRef
                } else {
                    ArgumentType::ByVal
                };
                let ident = self.consume(T![ident]);
                let arg_name = self.text(&ident).to_string();
                property_arguments.push((arg_name, argument_type));
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        }
        property_arguments
    }

    pub(crate) fn ident_part(&mut self) -> IdentPart {
        // example input: `foo` or `foo(1)` or `foo(1, 2) or Me`
        let peek = self.peek();
        if peek == T![me] {
            // TODO we might need to handle `Me` as a special node in the AST
            //   instead of going back to the string representation
            let me = self.consume(T![me]);
            return IdentPart {
                name: self.text(&me).to_string(),
                array_indices: Vec::new(),
            };
        }
        let ident = match peek {
            T![ident] => {
                let ident = self.consume(T![ident]);
                self.text(&ident).to_string()
            }
            T![property_access] => {
                // TODO we might need to handle property_access as a special node in the AST
                //   instead of going back to the string representation
                let property = self.consume(T![property_access]);
                self.text(&property).to_string()
            }
            other => {
                let full = self.peek_full();
                panic!(
                    "Expected identifier at line {}, column {} but found `{}`",
                    full.line, full.column, other
                );
            }
        };
        // TODO is `foo(1)(2)` valid syntax?
        let array_indices = self.parenthesized_arguments();
        IdentPart {
            name: ident,
            array_indices,
        }
    }

    pub(crate) fn ident_deep(&mut self) -> FullIdent {
        // example input: `foo(x + 1).bar.baz(2,3).name`
        let ident = self.ident_part();
        let mut property_accesses = vec![];
        // TODO should property access work with spaces? `foo . bar . baz`
        while self.at(T![property_access]) {
            let name = self.property();
            let array_indices = self.parenthesized_arguments();
            property_accesses.push(IdentPart {
                name,
                array_indices,
            });
        }
        FullIdent {
            base: ident,
            property_accesses,
        }
    }

    fn property(&mut self) -> String {
        let token = self.consume(T![property_access]);
        let property_name = self.text(&token).to_string();
        // validate first character and remover the dot
        assert_eq!(
            property_name.chars().next().unwrap(),
            '.',
            "Expected property access to start with a dot, but found `{}`",
            property_name
        );
        property_name[1..].to_string()
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
        let name = self.text(&ident).to_string();

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
