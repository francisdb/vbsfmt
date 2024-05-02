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
    pub(crate) fn consume(&mut self, expected: TokenKind) {
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
    }

    /// Check if the next token is some `kind` of token and consume it.
    pub(crate) fn consume_if_not_eof(&mut self, expected: TokenKind) {
        if self.peek() != T![EOF] {
            self.consume(expected);
        }
    }

    pub(crate) fn at_new_line_or_eof(&mut self) -> bool {
        matches!(self.peek(), T![nl] | T![EOF])
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
            if matches!(self.prev_token_kind, T![nl] | T![comment])
                && matches!(current_token.kind, T![nl])
            {
                self.prev_token_kind = current_token.kind;
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
    use crate::parser::ast::Expr::Ident;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn parse(input: &str) -> ast::Expr {
        let mut parser = Parser::new(input);
        parser.expression()
    }

    #[test]
    fn parse_expression() {
        // Weird spaces are to test that whitespace gets filtered out
        let expr = parse("42");
        assert_eq!(expr, ast::Expr::Literal(ast::Lit::Int(42)));
        let expr = parse("  2.7768");
        assert_eq!(expr, ast::Expr::Literal(ast::Lit::Float(2.7768)));
        let expr = parse(r#""I am a String!""#);
        assert_eq!(
            expr,
            ast::Expr::Literal(ast::Lit::Str("I am a String!".to_string()))
        );
        let expr = parse("foo");
        assert_eq!(expr, ast::Expr::Ident("foo".to_string()));
        let expr = parse("bar (  x, 2)");
        assert_eq!(
            expr,
            ast::Expr::FnCall {
                fn_name: "bar".to_string(),
                args: vec![
                    ast::Expr::Ident("x".to_string()),
                    ast::Expr::Literal(ast::Lit::Int(2)),
                ],
            }
        );
        let expr = parse("Not is_visible");
        assert_eq!(
            expr,
            ast::Expr::PrefixOp {
                op: T![not],
                expr: Box::new(ast::Expr::Ident("is_visible".to_string())),
            }
        );
        let expr = parse("(-13)");
        assert_eq!(
            expr,
            ast::Expr::PrefixOp {
                op: T![-],
                expr: Box::new(ast::Expr::Literal(ast::Lit::Int(13))),
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
            ast::Stmt::IfStmt {
                condition: Box::new(ast::Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Ident("x".to_string())),
                    rhs: Box::new(ast::Expr::Literal(ast::Lit::Int(2))),
                }),
                body: vec![ast::Stmt::Assignment {
                    var_name: "x".to_string(),
                    value: Box::new(ast::Expr::Literal(ast::Lit::Int(4))),
                }],
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
            ast::Stmt::WhileStmt {
                condition: Box::new(ast::Expr::InfixOp {
                    op: T![<],
                    lhs: Box::new(Ident("x".to_string())),
                    rhs: Box::new(ast::Expr::Literal(ast::Lit::Int(5))),
                }),
                body: vec![ast::Stmt::Assignment {
                    var_name: "x".to_string(),
                    value: Box::new(ast::Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Ident("x".to_string())),
                        rhs: Box::new(ast::Expr::Literal(ast::Lit::Int(1))),
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
            ast::Stmt::ForStmt {
                counter: "i".to_string(),
                start: Box::new(ast::Expr::Literal(ast::Lit::Int(1))),
                end: Box::new(ast::Expr::Literal(ast::Lit::Int(10))),
                step: None,
                body: vec![ast::Stmt::Assignment {
                    var_name: "x".to_string(),
                    value: Box::new(ast::Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Ident("x".to_string())),
                        rhs: Box::new(Ident("i".to_string())),
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
            ast::Item::Function {
                name: "add".to_string(),
                parameters: vec![
                    ast::Argument::ByVal("a".to_string()),
                    ast::Argument::ByVal("b".to_string())
                ],
                body: vec![ast::Stmt::Assignment {
                    var_name: "add".to_string(),
                    value: Box::new(ast::Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Ident("a".to_string())),
                        rhs: Box::new(Ident("b".to_string())),
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
            ast::Item::Sub {
                name: "log".to_string(),
                parameters: vec![
                    ast::Argument::ByVal("a".to_string()),
                    ast::Argument::ByVal("b".to_string())
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
                ast::Item::Sub {
                    name: "test".to_string(),
                    parameters: vec![ast::Argument::ByRef("a".to_string())],
                    body: vec![],
                },
                ast::Item::Function {
                    name: "test2".to_string(),
                    parameters: vec![ast::Argument::ByVal("a".to_string())],
                    body: vec![ast::Stmt::Assignment {
                        var_name: "test2".to_string(),
                        value: Box::new(ast::Expr::Ident("a".to_string())),
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
        assert_eq!(all, vec![ast::Item::OptionExplicit,]);
    }

    #[test]
    fn test_parse_no_arg_sub_call() {
        let input = "SayHello";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![ast::Item::Statement(ast::Stmt::SubCall {
                fn_name: "SayHello".to_string(),
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
                ast::Item::Statement(ast::Stmt::SubCall {
                    fn_name: "test".to_string(),
                    args: vec![],
                }),
                ast::Item::Statement(ast::Stmt::SubCall {
                    fn_name: "test".to_string(),
                    args: vec![ast::Expr::Literal(ast::Lit::Int(1))],
                }),
                ast::Item::Statement(ast::Stmt::SubCall {
                    fn_name: "test".to_string(),
                    args: vec![
                        ast::Expr::Literal(ast::Lit::Int(1)),
                        ast::Expr::Literal(ast::Lit::Int(2))
                    ],
                }),
            ]
        );
    }
}
