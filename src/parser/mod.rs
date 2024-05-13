use crate::{lexer::*, T};
use std::iter::Peekable;

pub mod ast;
mod expressions;
mod hierarchy;

pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
}

impl<'input> Parser<'input, TokenIter<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
        }
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    /// Get the source text of a token.
    pub fn text(&self, token: &Token) -> &'input str {
        token.text(self.input)
    }

    /// Look-ahead one token and see what kind of token it is.
    pub(crate) fn peek(&mut self) -> TokenKind {
        self.tokens
            .peek()
            .map(|token| token.kind)
            .unwrap_or(T![EOF])
    }

    pub(crate) fn peek_full(&mut self) -> &Token {
        self.tokens
            .peek()
            .unwrap_or_else(|| panic!("Expected a token, but found none"))
    }

    /// Check if the next token is some `kind` of token.
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Get the next token.
    pub(crate) fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    /// Move forward one token in the input and check
    /// that we pass the kind of token we expect.
    pub(crate) fn consume(&mut self, expected: TokenKind) -> Token {
        let token = self.next().unwrap_or_else(|| {
            panic!(
                "Expected to consume `{}`, but there was no next token",
                expected
            )
        });
        assert_eq!(
            token.kind, expected,
            "Expected to consume `{}` at line {}, column {}, but found `{}`",
            expected, token.line, token.column, token.kind
        );
        token
    }

    /// Check if the next token is some `kind` of token and consume it.
    pub(crate) fn consume_if_not_eof(&mut self, expected: TokenKind) {
        if self.peek() != T![EOF] {
            self.consume(expected);
        }
    }

    pub(crate) fn consume_line_delimiter(&mut self) {
        let peek = self.peek_full();
        match peek.kind {
            T![EOF] => {}
            T![nl] => {
                self.consume(T![nl]);
            }
            T![:] => {
                self.consume(T![:]);
            }
            other => panic!(
                "Unexpected token at line {}, column {}. Expected newline or colon, but found {}",
                peek.line, peek.column, other
            ),
        }
    }

    pub(crate) fn at_new_line_or_eof(&mut self) -> bool {
        matches!(self.peek(), T![nl] | T![:] | T![EOF])
    }
}

/// Iterator over the tokens of the lexer, filtering out whitespace, empty lines and comments.
pub struct TokenIter<'input> {
    lexer: Lexer<'input>,
    prev_token_kind: TokenKind,
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Lexer::new(input),
            prev_token_kind: T![nl],
        }
    }
}

impl<'input> Iterator for TokenIter<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let current_token = self.lexer.next()?;
            // ignore whitespace
            if matches!(current_token.kind, T![ws]) {
                continue;
            }
            // skip empty lines
            if matches!(self.prev_token_kind, T![nl]) && matches!(current_token.kind, T![nl]) {
                self.prev_token_kind = current_token.kind;
                continue;
            }
            // skip single line comments that are preceded by a newline
            if matches!(self.prev_token_kind, T![nl]) && matches!(current_token.kind, T![comment]) {
                // hacky way to not keep the comment newline
                self.prev_token_kind = T![nl];
                continue;
            }
            self.prev_token_kind = current_token.kind;
            if !matches!(current_token.kind, T![comment]) {
                return Some(current_token);
            } // else continue
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::ast::ErrorClause::{Goto0, ResumeNext};
    use crate::parser::ast::Stmt::OnError;
    use crate::parser::ast::{Argument, Expr, FullIdent, IdentPart, Item, Lit, SetRhs, Stmt};
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn parse(input: &str) -> Expr {
        let mut parser = Parser::new(input);
        parser.expression()
    }

    #[test]
    fn test_token_iter_newlines_with_comments() {
        let input = indoc! {r#"
Const a = 1			' some info
					' info continued
					' info further continued
        "#};
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .iter()
            .map(|t| t.kind)
            .filter(|t| t != &T![ws])
            .collect();
        assert_eq!(
            tokens,
            [
                T![const],
                T![ident],
                T![=],
                T![integer_literal],
                T![comment],
                T![nl],
                T![comment],
                T![nl],
                T![comment],
                T![nl],
                T![EOF],
            ]
        );

        let token_iter = TokenIter::new(input);
        let tokens: Vec<TokenKind> = token_iter.map(|t| t.kind).collect();
        assert_eq!(
            tokens,
            [
                T![const],
                T![ident],
                T![=],
                T![integer_literal],
                T![nl],
                T![EOF],
            ]
        );
    }

    #[test]
    fn parse_expression() {
        // Weird spaces are to test that whitespace gets filtered out
        let expr = parse("42");
        assert_eq!(expr, Expr::Literal(Lit::Int(42)));
        let expr = parse("  2.7768");
        assert_eq!(expr, Expr::Literal(Lit::Float(2.7768)));
        let expr = parse(r#""I am a String!""#);
        assert_eq!(expr, Expr::Literal(Lit::Str("I am a String!".to_string())));
        let expr = parse("foo");
        assert_eq!(expr, ast::Expr::ident("foo"));
        let expr = parse("bar (  x, 2)");
        assert_eq!(
            expr,
            Expr::IdentFnSubCall(FullIdent {
                base: IdentPart {
                    name: "bar".to_string(),
                    array_indices: vec![Expr::ident("x".to_string()), Expr::Literal(Lit::Int(2)),],
                },
                property_accesses: vec![],
            },)
        );
        let expr = parse("Not is_visible");
        assert_eq!(
            expr,
            Expr::PrefixOp {
                op: T![not],
                expr: Box::new(Expr::ident("is_visible".to_string())),
            }
        );
        let expr = parse("(-13)");
        assert_eq!(
            expr,
            Expr::PrefixOp {
                op: T![-],
                expr: Box::new(Expr::Literal(Lit::Int(13))),
            }
        );
    }

    #[test]
    fn parse_binary_expressions() {
        let expr = parse("4 + 2 * 3");
        assert_eq!(expr.to_string(), "(4 + (2 * 3))"); // passes

        let expr = parse("4 * 2 + 3");
        assert_eq!(expr.to_string(), "((4 * 2) + 3)"); // fails

        let expr = parse("4 - 2 - 3");
        assert_eq!(expr.to_string(), "((4 - 2) - 3)"); // fails

        let expr = parse("4 ^ 2 ^ 3");
        assert_eq!(expr.to_string(), "(4 ^ (2 ^ 3))"); // passes

        let expr = parse(r#"45.7 + 3 + 5 * 4^8^9 / 6 > 4 and test - 7 / 4 = "Hallo""#);
        assert_eq!(
            expr.to_string(),
            r#"((((45.7 + 3) + ((5 * (4 ^ (8 ^ 9))) / 6)) > 4) and ((test - (7 / 4)) = "Hallo"))"#
        );

        let expr = parse("2.0 / ((3.0 + 4.0) * (5.0 - 6.0)) * 7.0");
        assert_eq!(expr.to_string(), "((2 / ((3 + 4) * (5 - 6))) * 7)");

        let expr = parse("min ( test + 4 , sin(2*PI ))");
        assert_eq!(expr.to_string(), "min((test + 4), sin((2 * PI)))");
    }

    #[test]
    fn parse_simple_if_stmt() {
        let input = indoc! {r#"
            if x > 2 then
                x = 4
            end if
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x".to_string())),
                    rhs: Box::new(Expr::Literal(Lit::Int(2))),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::Literal(Lit::Int(4))),
                }],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn parse_simple_while() {
        let input = indoc! {r#"
            While x < 5
                x=x+1
            Wend
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::WhileStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![<],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::Literal(Lit::Int(5))),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("x".to_string())),
                        rhs: Box::new(Expr::Literal(Lit::Int(1))),
                    }),
                },],
            }
        );
    }

    #[test]
    fn parse_simple_for_next_loop() {
        let input = indoc! {r#"
            For i = 1 to 10
                x = x + i
            Next
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForStmt {
                counter: "i".to_string(),
                start: Box::new(Expr::Literal(Lit::Int(1))),
                end: Box::new(Expr::Literal(Lit::Int(10))),
                step: None,
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("x".to_string())),
                        rhs: Box::new(Expr::ident("i".to_string())),
                    }),
                }],
            }
        );
    }

    #[test]
    fn parse_simple_for_next_loop_with_exit() {
        let input = indoc! {r#"
            For i = 1 to 10
                x = x * i
                If x > 10 Then Exit For
            Next
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForStmt {
                counter: "i".to_string(),
                start: Box::new(Expr::Literal(Lit::Int(1))),
                end: Box::new(Expr::Literal(Lit::Int(10))),
                step: None,
                body: vec![
                    Stmt::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(Expr::InfixOp {
                            op: T![*],
                            lhs: Box::new(Expr::ident("x".to_string())),
                            rhs: Box::new(Expr::ident("i".to_string())),
                        }),
                    },
                    Stmt::IfStmt {
                        condition: Box::new(Expr::InfixOp {
                            op: T![>],
                            lhs: Box::new(Expr::ident("x".to_string())),
                            rhs: Box::new(Expr::Literal(Lit::Int(10))),
                        }),
                        body: vec![Stmt::ExitFor],
                        elseif_statements: vec![],
                        else_stmt: None,
                    },
                ],
            }
        );
    }

    #[test]
    fn parse_for_step() {
        let input = indoc! {r#"
            For i = For_nr to Next_nr step Bdir
                ' do nothing
            Next
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        assert_eq!(
            file,
            vec![Item::Statement(Stmt::ForStmt {
                counter: "i".to_string(),
                start: Box::new(Expr::ident("For_nr".to_string())),
                end: Box::new(Expr::ident("Next_nr".to_string())),
                step: Some(Box::new(Expr::ident("Bdir".to_string()))),
                body: vec![],
            }),]
        );
    }

    #[test]
    fn parse_for_inline() {
        let input = "For x = 1 To PlayerMode(currentplayer)+1 : Blink(x,1)=1 : Next";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForStmt {
                counter: "x".to_string(),
                start: Box::new(Expr::Literal(Lit::Int(1))),
                end: Box::new(Expr::InfixOp {
                    op: T![+],
                    lhs: Box::new(Expr::IdentFnSubCall(FullIdent {
                        base: IdentPart {
                            name: "PlayerMode".to_string(),
                            array_indices: vec![Expr::IdentFnSubCall(FullIdent {
                                base: IdentPart::ident("currentplayer"),
                                property_accesses: vec![],
                            }),],
                        },
                        property_accesses: vec![],
                    })),
                    rhs: Box::new(Expr::Literal(Lit::Int(1))),
                }),
                step: None,
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent {
                        base: IdentPart {
                            name: "Blink".to_string(),
                            array_indices: vec![Expr::ident("x"), Expr::Literal(Lit::Int(1))],
                        },
                        property_accesses: vec![],
                    },
                    value: Box::new(Expr::Literal(Lit::Int(1))),
                },],
            }
        );
    }

    #[test]
    fn parse_foreach_multiline() {
        let input = indoc! {r#"
            For each dog in dogs
                dog.visible = true
            Next
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForEachStmt {
                element: "dog".to_string(),
                group: Box::new(Expr::ident("dogs".to_string())),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent {
                        base: IdentPart::ident("dog"),
                        property_accesses: vec![IdentPart::ident("visible")],
                    },
                    value: Box::new(Expr::Literal(Lit::Bool(true))),
                }],
            }
        );
    }

    #[test]
    fn parse_foreach_inline() {
        let input = indoc! {r#"
            For each dog in dogs : dog.volume = 0 : dog.visible = true : Next
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForEachStmt {
                element: "dog".to_string(),
                group: Box::new(Expr::ident("dogs".to_string())),
                body: vec![
                    Stmt::Assignment {
                        full_ident: FullIdent {
                            base: IdentPart::ident("dog"),
                            property_accesses: vec![IdentPart::ident("volume")],
                        },
                        value: Box::new(Expr::Literal(Lit::Int(0))),
                    },
                    Stmt::Assignment {
                        full_ident: FullIdent {
                            base: IdentPart::ident("dog"),
                            property_accesses: vec![IdentPart::ident("visible")],
                        },
                        value: Box::new(Expr::Literal(Lit::Bool(true))),
                    },
                ],
            }
        );
    }

    #[test]
    fn parse_simple_function_declaration() {
        let input = indoc! {r#"
            Function add (a, b)
                add = a + b
            End Function
        "#};
        let mut parser = Parser::new(input);
        let item = parser.item();
        assert_eq!(
            item,
            Item::Function {
                name: "add".to_string(),
                parameters: vec![
                    Argument::ByVal("a".to_string()),
                    Argument::ByVal("b".to_string())
                ],
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("add"),
                    value: Box::new(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("a")),
                        rhs: Box::new(Expr::ident("b")),
                    }),
                }],
            }
        );
    }

    #[test]
    fn parse_sub_declaration() {
        let input = indoc! {r#"
            Sub log (a, b)
                'print a
                'print b
            End Sub
        "#};
        let mut parser = Parser::new(input);
        let item = parser.item();
        assert_eq!(
            item,
            Item::Sub {
                name: "log".to_string(),
                parameters: vec![
                    Argument::ByVal("a".to_string()),
                    Argument::ByVal("b".to_string())
                ],
                body: vec![],
            }
        );
    }

    #[test]
    fn parse_empty_sub_without_args() {
        let input = indoc! {r#"
            Sub log
            End Sub
        "#};
        let mut parser = Parser::new(input);
        let item = parser.item();
        assert_eq!(
            item,
            Item::Sub {
                name: "log".to_string(),
                parameters: vec![],
                body: vec![],
            }
        );
    }

    #[test]
    fn parse_inline_sub_declaration() {
        let input = "Sub Trigger003_hit : RampWireRight.x = 0.1 : Light030.state = 0 : End Sub";
        let mut parser = Parser::new(input);
        let item = parser.item();
        assert_eq!(
            item,
            Item::Sub {
                name: "Trigger003_hit".to_string(),
                parameters: vec![],
                body: vec![
                    Stmt::Assignment {
                        full_ident: FullIdent {
                            base: IdentPart::ident("RampWireRight"),
                            property_accesses: vec![IdentPart::ident("x")],
                        },
                        value: Box::new(Expr::Literal(Lit::Float(0.1))),
                    },
                    Stmt::Assignment {
                        full_ident: FullIdent {
                            base: IdentPart::ident("Light030"),
                            property_accesses: vec![IdentPart::ident("state")],
                        },
                        value: Box::new(Expr::Literal(Lit::Int(0))),
                    },
                ],
            }
        );
    }

    #[test]
    fn parse_byval_byref() {
        let input = indoc! {r#"
            Sub test (ByRef a)
                'print a
            End Sub
            Function test2 (ByVal a)
                test2 = a
            End Function
        "#};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Sub {
                    name: "test".to_string(),
                    parameters: vec![Argument::ByRef("a".to_string())],
                    body: vec![],
                },
                Item::Function {
                    name: "test2".to_string(),
                    parameters: vec![Argument::ByVal("a".to_string())],
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("test2"),
                        value: Box::new(Expr::ident("a".to_string())),
                    }],
                },
            ]
        );
    }

    #[test]
    fn test_parse_file_empty() {
        let input = "";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_newlines() {
        let input = "\r\n\n\n\r\n";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_comments() {
        let input = indoc! {"
            ' This is a comment

            ' This is another comment without newline"};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_statement_with_trailing_comment() {
        let input = indoc! {"
            Option Explicit ' Force explicit variable declaration.
            ' This is another comment"};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![Item::OptionExplicit,]);
    }

    #[test]
    fn test_parse_no_arg_sub_call() {
        let input = "SayHello";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![Item::Statement(Stmt::SubCall {
                fn_name: FullIdent::ident("SayHello"),
                args: vec![],
            }),]
        );
    }

    #[test]
    fn test_parse_sub_call() {
        let input = indoc! {"
            test
            test 1
            test 1, 2"
        };
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Statement(Stmt::SubCall {
                    fn_name: FullIdent::ident("test"),
                    args: vec![],
                }),
                Item::Statement(Stmt::SubCall {
                    fn_name: FullIdent::ident("test"),
                    args: vec![Some(Expr::Literal(Lit::Int(1)))],
                }),
                Item::Statement(Stmt::SubCall {
                    fn_name: FullIdent::ident("test"),
                    args: vec![
                        Some(Expr::Literal(Lit::Int(1))),
                        Some(Expr::Literal(Lit::Int(2)))
                    ],
                }),
            ]
        );
    }

    #[test]
    fn test_parse_error_handling() {
        let input = indoc! {"
            On Error Resume Next
            On Error GoTo 0
        "};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Statement(OnError {
                    error_clause: ResumeNext
                }),
                Item::Statement(OnError {
                    error_clause: Goto0
                }),
            ]
        );
    }

    #[test]
    fn test_single_line_if() {
        let input = r#"If Err Then MsgBox "Oh noes""#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::ident("Err".to_string())),
                body: vec![Stmt::SubCall {
                    fn_name: FullIdent::ident("MsgBox"),
                    args: vec![Some(Expr::Literal(Lit::Str("Oh noes".to_string())))],
                }],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn test_single_line_if_multi_statement() {
        let input = r#"If Err Then MsgBox "Oh noes": MsgBox "Crash""#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::ident("Err".to_string())),
                body: vec![
                    Stmt::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Some(Expr::Literal(Lit::Str("Oh noes".to_string())))],
                    },
                    Stmt::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Some(Expr::Literal(Lit::Str("Crash".to_string())))],
                    }
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn test_singe_line_if_with_end_if() {
        let input = r#"if VRRoom > 0 Then bbs006.state = x2 Else controller.B2SSetData 50,x2 : controller.B2SSetData 53,x2 : End If"#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("VRRoom".to_string())),
                    rhs: Box::new(Expr::Literal(Lit::Int(0))),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent {
                        base: IdentPart::ident("bbs006"),
                        property_accesses: vec![IdentPart::ident("state")],
                    },
                    value: Box::new(Expr::ident("x2".to_string())),
                }],
                elseif_statements: vec![],
                else_stmt: Some(vec![
                    Stmt::SubCall {
                        fn_name: FullIdent {
                            base: IdentPart::ident("controller"),
                            property_accesses: vec![IdentPart::ident("B2SSetData")],
                        },
                        args: vec![
                            Some(Expr::Literal(Lit::Int(50))),
                            Some(Expr::ident("x2".to_string())),
                        ],
                    },
                    Stmt::SubCall {
                        fn_name: FullIdent {
                            base: IdentPart::ident("controller"),
                            property_accesses: vec![IdentPart::ident("B2SSetData")],
                        },
                        args: vec![
                            Some(Expr::Literal(Lit::Int(53))),
                            Some(Expr::ident("x2".to_string())),
                        ],
                    },
                ]),
            }
        );
    }

    #[test]
    fn test_parse_if_two_single_lines() {
        let input = r#"
            If(x <> "") Then LutValue = CDbl(x) Else LutValue = 1
	        If LutValue < 1 Then LutValue = 1
        "#;
        let mut parser = Parser::new(input);
        let file = parser.file();
        assert_eq!(
            file,
            vec![
                Item::Statement(Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![<>],
                        lhs: Box::new(Expr::ident("x".to_string())),
                        rhs: Box::new(Expr::Literal(Lit::Str("".to_string()))),
                    }),
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::IdentFnSubCall(FullIdent {
                            base: IdentPart {
                                name: "CDbl".to_string(),
                                array_indices: vec![Expr::ident("x".to_string())],
                            },
                            property_accesses: vec![],
                        })),
                    }],
                    elseif_statements: vec![],
                    else_stmt: Some(vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::Literal(Lit::Int(1))),
                    }]),
                }),
                Item::Statement(Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![<],
                        lhs: Box::new(Expr::ident("LutValue".to_string())),
                        rhs: Box::new(Expr::Literal(Lit::Int(1))),
                    }),
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::Literal(Lit::Int(1))),
                    }],
                    elseif_statements: vec![],
                    else_stmt: None,
                }),
            ]
        );
    }

    #[test]
    fn test_parse_if_single_line_with_else() {
        let input = r#"If x > 2 Then y = 3 Else y = 4"#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x".to_string())),
                    rhs: Box::new(Expr::Literal(Lit::Int(2))),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("y"),
                    value: Box::new(Expr::Literal(Lit::Int(3))),
                }],
                elseif_statements: vec![],
                else_stmt: Some(vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("y"),
                    value: Box::new(Expr::Literal(Lit::Int(4))),
                }]),
            }
        );
    }

    #[test]
    fn test_if_nested_single_line() {
        let input = indoc! {r#"
        If x > 2 Then
            If This Or That Then DoSomething 'weird comment
        End If
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.file();
        assert_eq!(
            stmt,
            vec![Item::Statement(Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x".to_string())),
                    rhs: Box::new(Expr::Literal(Lit::Int(2))),
                }),
                body: vec![Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![or],
                        lhs: Box::new(Expr::ident("This".to_string())),
                        rhs: Box::new(Expr::ident("That".to_string())),
                    }),
                    body: vec![Stmt::SubCall {
                        fn_name: FullIdent::ident("DoSomething"),
                        args: vec![],
                    }],
                    elseif_statements: vec![],
                    else_stmt: None,
                }],
                elseif_statements: vec![],
                else_stmt: None,
            }),]
        );
    }

    #[test]
    fn parse_dim() {
        let input = "Dim x";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(stmt, Stmt::dim("x"));
    }

    #[test]
    fn test_dim_array() {
        let input = "Dim x(1, 2)";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Dim {
                vars: vec![(
                    "x".to_string(),
                    vec![Expr::Literal(Lit::Int(1)), Expr::Literal(Lit::Int(2))]
                )],
            }
        );
    }

    #[test]
    fn test_dim_array_with_space() {
        let input = "Dim PlayerMode (2)";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Dim {
                vars: vec![("PlayerMode".to_string(), vec![Expr::Literal(Lit::Int(2))])],
            }
        );
    }

    #[test]
    fn parse_dim_multiple() {
        let input = "Dim x,y, z(1 + 3)";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Dim {
                vars: vec![
                    ("x".to_string(), vec![]),
                    ("y".to_string(), vec![]),
                    (
                        "z".to_string(),
                        vec![Expr::InfixOp {
                            op: T![+],
                            lhs: Box::new(Expr::Literal(Lit::Int(1))),
                            rhs: Box::new(Expr::Literal(Lit::Int(3))),
                        }]
                    ),
                ],
            }
        );
    }

    #[test]
    fn parse_const() {
        let input = "Const x = 42";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Const {
                var_name: "x".to_string(),
                value: Box::new(Expr::Literal(Lit::Int(42))),
            }
        );
    }

    #[test]
    fn parse_two_consts_with_comments() {
        let input = indoc! {"
            Const x = 42 ' The answer to everything
            Const y = 13 ' An unlucky number
        "};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Statement(Stmt::Const {
                    var_name: "x".to_string(),
                    value: Box::new(Expr::Literal(Lit::Int(42))),
                }),
                Item::Statement(Stmt::Const {
                    var_name: "y".to_string(),
                    value: Box::new(Expr::Literal(Lit::Int(13))),
                }),
            ]
        );
    }

    #[test]
    fn parse_true_false() {
        let input = indoc! {r#"
            Const Test = False
            Const Test2 = True
        "#};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Statement(Stmt::Const {
                    var_name: "Test".to_string(),
                    value: Box::new(Expr::Literal(Lit::Bool(false))),
                }),
                Item::Statement(Stmt::Const {
                    var_name: "Test2".to_string(),
                    value: Box::new(Expr::Literal(Lit::Bool(true))),
                }),
            ]
        );
    }

    #[test]
    fn parse_block_with_colons() {
        let input = indoc! {r#"
            Dim test
            If RenderingMode = 2 Then
                test = 1 : startcontroller
            elseif RenderingMode = 3 Then
                test = 2 : startcontroller
            else
                test = 0
            End If
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![
                Item::Statement(Stmt::dim("test")),
                Item::Statement(Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![=],
                        lhs: Box::new(Expr::ident("RenderingMode")),
                        rhs: Box::new(Expr::Literal(Lit::Int(2))),
                    }),
                    body: vec![
                        Stmt::Assignment {
                            full_ident: FullIdent::ident("test"),
                            value: Box::new(Expr::Literal(Lit::Int(1))),
                        },
                        Stmt::SubCall {
                            fn_name: FullIdent::ident("startcontroller"),
                            args: vec![],
                        }
                    ],
                    elseif_statements: vec![(
                        Box::new(Expr::InfixOp {
                            op: T![=],
                            lhs: Box::new(Expr::ident("RenderingMode")),
                            rhs: Box::new(Expr::Literal(Lit::Int(3))),
                        }),
                        vec![
                            Stmt::Assignment {
                                full_ident: FullIdent::ident("test"),
                                value: Box::new(Expr::Literal(Lit::Int(2))),
                            },
                            Stmt::SubCall {
                                fn_name: FullIdent::ident("startcontroller"),
                                args: vec![],
                            }
                        ]
                    )],
                    else_stmt: Some(vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("test"),
                        value: Box::new(Expr::Literal(Lit::Int(0))),
                    }]),
                }),
            ]
        );
    }

    #[test]
    fn test_assignment_to_object_in_array_property() {
        let input = r#"objectArray(i).image = "test""#;
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Assignment {
                full_ident: FullIdent {
                    base: IdentPart {
                        name: "objectArray".to_string(),
                        array_indices: vec![Expr::ident("i".to_string())],
                    },
                    property_accesses: vec![IdentPart {
                        name: "image".to_string(),
                        array_indices: vec![],
                    }],
                },
                value: Box::new(Expr::Literal(Lit::Str("test".to_string()))),
            })]
        );
    }

    #[test]
    fn test_assignment_with_property_in_expression() {
        let input = "foo.a = foo.b-120.5";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Assignment {
                full_ident: FullIdent {
                    base: IdentPart::ident("foo"),
                    property_accesses: vec![IdentPart::ident("a"),],
                },
                value: Box::new(Expr::InfixOp {
                    op: T![-],
                    lhs: Box::new(Expr::IdentFnSubCall(FullIdent {
                        base: IdentPart::ident("foo"),
                        property_accesses: vec![IdentPart::ident("b")],
                    })),
                    rhs: Box::new(Expr::Literal(Lit::Float(120.5))),
                }),
            })]
        );
    }

    #[test]
    fn test_object_assignment_using_new() {
        let input = "Set foo = New Bar";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Set {
                var_name: "foo".to_string(),
                rhs: SetRhs::new_class("Bar"),
            })]
        );
    }

    #[test]
    fn test_parse_select() {
        let input = indoc! {r#"
            Select Case x
                Case 1, 2
                    y = 2
                Case 3
                    y = 3
                Case Else
                    y = 4
            End Select
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::SelectCase {
                test_expr: Box::new(Expr::ident("x".to_string())),
                cases: vec![
                    (
                        vec![Expr::Literal(Lit::Int(1)), Expr::Literal(Lit::Int(2)),],
                        vec![Stmt::Assignment {
                            full_ident: FullIdent::ident("y"),
                            value: Box::new(Expr::Literal(Lit::Int(2))),
                        }]
                    ),
                    (
                        vec![Expr::Literal(Lit::Int(3))],
                        vec![Stmt::Assignment {
                            full_ident: FullIdent::ident("y"),
                            value: Box::new(Expr::Literal(Lit::Int(3))),
                        }]
                    ),
                ],
                else_stmt: Some(vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("y"),
                    value: Box::new(Expr::Literal(Lit::Int(4))),
                }]),
            })]
        );
    }

    #[test]
    fn test_parse_select_inline_cases() {
        let input = indoc! {r#"
            Select Case x
                Case 1, 2:y = 2
                Case Else: y = 4
            End Select
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::SelectCase {
                test_expr: Box::new(Expr::ident("x".to_string())),
                cases: vec![(
                    vec![Expr::Literal(Lit::Int(1)), Expr::Literal(Lit::Int(2)),],
                    vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("y"),
                        value: Box::new(Expr::Literal(Lit::Int(2))),
                    }],
                ),],
                else_stmt: Some(vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("y"),
                    value: Box::new(Expr::Literal(Lit::Int(4))),
                }]),
            })]
        );
    }

    #[test]
    fn test_parse_is() {
        let input = indoc! {r#"
            If Not Controller Is Nothing Then ' Controller might no be there
                Controller.Run
            End If
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::PrefixOp {
                    op: T![not],
                    expr: Box::new(Expr::InfixOp {
                        op: T![is],
                        lhs: Box::new(Expr::ident("Controller".to_string())),
                        rhs: Box::new(Expr::Literal(Lit::Nothing)),
                    }),
                }),
                body: vec![Stmt::SubCall {
                    fn_name: FullIdent {
                        base: IdentPart::ident("Controller"),
                        property_accesses: vec![IdentPart::ident("Run")],
                    },
                    args: vec![],
                },],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn test_sub_call_with_empty_args() {
        let input = r#"DoSomething 1,,"test""#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                args: vec![
                    Some(Expr::Literal(Lit::Int(1))),
                    None,
                    Some(Expr::Literal(Lit::Str("test".to_string()))),
                ],
            }
        );
    }
}
