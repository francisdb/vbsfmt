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
    pub fn text(&self, token: Token) -> &'input str {
        token.text(self.input)
    }

    /// Look-ahead one token and see what kind of token it is.
    pub(crate) fn peek(&mut self) -> TokenKind {
        self.tokens
            .peek()
            .map(|token| token.kind)
            .unwrap_or(T![EOF])
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
            "Expected to consume `{}`, but found `{}`",
            expected, token.kind
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
        let peek = self.peek();
        match peek {
            T![EOF] => {}
            T![nl] => {
                self.consume(T![nl]);
            }
            T![:] => {
                self.consume(T![:]);
            }
            _ => panic!("Expected newline or colon, but found {:?}", peek),
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
            // skip empty lines
            if matches!(self.prev_token_kind, T![nl]) && matches!(current_token.kind, T![nl]) {
                self.prev_token_kind = current_token.kind;
                continue;
            }
            // skip single line comments that are preceded by a newline
            if matches!(self.prev_token_kind, T![nl]) && matches!(current_token.kind, T![comment]) {
                // hacky way to not keep the comment newline
                // TODO write a test case for this iterator
                self.prev_token_kind = T![nl];
                continue;
            }
            self.prev_token_kind = current_token.kind;
            if !matches!(current_token.kind, T![ws] | T![comment]) {
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
    use crate::parser::ast::{Argument, Expr, FullIdent, Item, Lit, Stmt};
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn parse(input: &str) -> Expr {
        let mut parser = Parser::new(input);
        parser.expression()
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
            Expr::FnCall {
                fn_name: FullIdent::ident("bar"),
                args: vec![
                    Expr::ident("x".to_string()),
                    Expr::Literal(ast::Lit::Int(2)),
                ],
            }
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
        assert_eq!(expr.to_string(), "min((test + 4),sin((2 * PI),),)");
    }

    #[test]
    fn parse_simple_if_stmt() {
        let input = indoc! {r#"
            if x > 2 then
                x = 4
            end if
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement();
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x".to_string())),
                    rhs: Box::new(Expr::Literal(Lit::Int(2))),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent {
                        name: "x".to_string(),
                        property_accesses: vec![],
                    },
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
        let stmt = parser.statement();
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
        let stmt = parser.statement();
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
                    full_ident: FullIdent {
                        name: "add".to_string(),
                        property_accesses: vec![],
                    },
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
    fn parse_simple_sub_declaration() {
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
                    args: vec![Expr::Literal(Lit::Int(1))],
                }),
                Item::Statement(Stmt::SubCall {
                    fn_name: FullIdent::ident("test"),
                    args: vec![Expr::Literal(Lit::Int(1)), Expr::Literal(Lit::Int(2))],
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
        let stmt = parser.statement();
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::ident("Err".to_string())),
                body: vec![Stmt::SubCall {
                    fn_name: FullIdent::ident("MsgBox"),
                    args: vec![Expr::Literal(Lit::Str("Oh noes".to_string()))],
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
        let stmt = parser.statement();
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::ident("Err".to_string())),
                body: vec![
                    Stmt::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Expr::Literal(Lit::Str("Oh noes".to_string()))],
                    },
                    Stmt::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Expr::Literal(Lit::Str("Crash".to_string()))],
                    }
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn parse_dim() {
        let input = "Dim x";
        let mut parser = Parser::new(input);
        let stmt = parser.statement();
        assert_eq!(stmt, Stmt::dim("x"));
    }

    #[test]
    fn test_dim_array() {
        let input = "Dim x(1, 2)";
        let mut parser = Parser::new(input);
        let stmt = parser.statement();
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
        let stmt = parser.statement();
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
        let stmt = parser.statement();
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
        let stmt = parser.statement();
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
    fn test_assignment_with_property_in_expression() {
        let input = "foo.a = foo.b-120.5";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Assignment {
                full_ident: FullIdent {
                    name: "foo".to_string(),
                    property_accesses: vec!["a".to_string()],
                },
                value: Box::new(Expr::InfixOp {
                    op: T![-],
                    lhs: Box::new(Expr::Ident(FullIdent {
                        name: "foo".to_string(),
                        property_accesses: vec!["b".to_string()],
                    })),
                    rhs: Box::new(Expr::Literal(Lit::Float(120.5))),
                }),
            })]
        );
    }
}
