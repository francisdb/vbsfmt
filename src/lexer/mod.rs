use crate::lexer::generated::LogosToken;
use logos::Logos;
pub use token::{Token, TokenKind};

use crate::lexer::rules::{unambiguous_single_char, Rule};
use crate::lexer::token::Span;
use crate::lexer::TokenKind::ParseError;
use crate::T;

mod generated;
mod rules;
mod token;

//pub type Lexer<'input> = CustomLexer<'input>;
pub type Lexer<'input> = LogosLexer<'input>;

pub struct LogosLexer<'input> {
    generated: logos::SpannedIter<'input, LogosToken>,
    eof: bool,
}

impl<'input> LogosLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            generated: LogosToken::lexer(input).spanned(),
            eof: false,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }
}

impl<'input> Iterator for LogosLexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.generated.next() {
            Some((token_result, span)) => match token_result {
                Ok(token) => {
                    let (line, column) = token.line_column();
                    //println!("{}: {}:{}", token.kind(), line, column);
                    Some(Token {
                        kind: token.kind(),
                        span: span.into(),
                        line,
                        column,
                    })
                }
                Err(_) => {
                    // TODO can we provide more information here?
                    //   can we get the line and column number?
                    Some(Token {
                        kind: ParseError,
                        span: span.into(),
                        line: 0,
                        column: 0,
                    })
                }
            },
            None if self.eof => None,
            None => {
                self.eof = true;
                // TODO can we provide more information here?
                //   can we get the line and column number?
                Some(Token {
                    kind: T![EOF],
                    span: (0..0).into(),
                    line: 0,
                    column: 0,
                })
            }
        }
    }
}

pub struct CustomLexer<'input> {
    input: &'input str,
    position: u32,
    eof: bool,
    rules: Vec<Rule>,
}

impl<'input> CustomLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            position: 0,
            eof: false,
            rules: rules::get_rules(),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }

    pub fn next_token(&mut self, input: &str) -> Token {
        self.valid_token(input)
            .unwrap_or_else(|| self.invalid_token(input))
    }

    /// Returns `None` if the lexer cannot find a token at the start of `input`.
    fn valid_token(&mut self, input: &str) -> Option<Token> {
        let next = input.chars().next().unwrap();
        let (len, kind) = if is_vbs_whitespace(&next) {
            (
                input
                    .char_indices()
                    .take_while(|(_, c)| is_vbs_whitespace(c))
                    .last()
                    .unwrap() // we know there is at least one whitespace character
                    .0 as u32
                    + 1,
                T![ws],
            )
        } else if let Some(kind) = unambiguous_single_char(next) {
            (1, kind)
        } else {
            self.rules
                .iter()
                // `max_by_key` returns the last element if multiple
                // rules match, but we want earlier rules to "win"
                // against later ones
                .rev()
                .filter_map(|rule| Some(((rule.matches)(input)?, rule.kind)))
                .max_by_key(|&(len, _)| len)?
        };

        let start = self.position;
        self.position += len;
        Some(Token {
            kind,
            span: Span {
                start,
                end: start + len,
            },
            line: 0,
            column: 0,
        })
    }

    /// Always "succeeds", because it creates an error `Token`.
    fn invalid_token(&mut self, input: &str) -> Token {
        let start = self.position; // <- NEW!
        let len = input
            .char_indices()
            .find(|(pos, _)| self.valid_token(&input[*pos..]).is_some())
            .map(|(pos, _)| pos)
            .unwrap_or_else(|| input.len());
        debug_assert!(len <= input.len());

        let len = len as u32;
        self.position = start + len;
        // println!(
        //     "Invalid token: {:?}",
        //     &input[start as usize..(start + len) as usize]
        // );
        Token {
            kind: T![error],
            span: Span {
                start,
                end: start + len,
            },
            line: 0,
            column: 0,
        }
    }
}

/// Returns `true` if the given character is a whitespace character in VBScript.
/// But we explicitly exclude newlines, because they are significant in VBScript.
/// CRLF, LF, and CR are all treated as newlines.
fn is_vbs_whitespace(next: &char) -> bool {
    if matches!(*next, '\n' | '\r') {
        return false;
    }
    next.is_whitespace()
}

impl<'input> Iterator for CustomLexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position as usize >= self.input.len() {
            if self.eof {
                return None;
            }
            self.eof = true;
            Some(Token {
                kind: T![EOF],
                span: Span {
                    start: self.position,
                    end: self.position,
                },
                line: 0,
                column: 0,
            })
        } else {
            Some(self.next_token(&self.input[self.position as usize..]))
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{Lexer, Token};
    use crate::T;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_is_vbs_whitespace() {
        assert!(super::is_vbs_whitespace(&' '));
        assert!(super::is_vbs_whitespace(&'\t'));
        assert!(super::is_vbs_whitespace(&'\u{00A0}'));
        assert!(super::is_vbs_whitespace(&'\u{2000}'));
        assert!(super::is_vbs_whitespace(&'\u{3000}'));
        //assert!(super::is_vbs_whitespace(&'\u{FEFF}'));
        assert!(super::is_vbs_whitespace(&'\u{2028}'));
        assert!(super::is_vbs_whitespace(&'\u{2029}'));
        assert!(!super::is_vbs_whitespace(&'\n'));
        assert!(!super::is_vbs_whitespace(&'\r'));
    }

    #[test]
    fn test_string_literal() {
        let input = r#""hello world""#;
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![string_literal], T![EOF],]);
    }

    #[test]
    fn test_lexer_options() {
        let input = indoc! {r#"
        Option Explicit
    "#};
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [T![option], T![ws], T![ident], T![nl], T![EOF],]
        );
    }

    #[test]
    fn test_lexer_comment_with_pipe() {
        let input = "' |\n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .into_iter()
            .filter(|t| t.kind != T![ws])
            .collect();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![comment], T![nl], T![EOF],]);
    }

    #[test]
    fn test_lexer_rem_comment() {
        let input = "REM comment here\n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .into_iter()
            .filter(|t| t.kind != T![ws])
            .collect();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![comment], T![nl], T![EOF],]);
    }

    #[test]
    fn test_lexer_not_rem() {
        // this used to lexed as a comment
        let input = "Private Sub RemoveBall(aBall)";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .into_iter()
            .filter(|t| t.kind != T![ws])
            .collect();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![private],
                T![sub],
                T![ident],
                T!['('],
                T![ident],
                T![')'],
                T![EOF],
            ]
        );
    }

    #[test]
    fn test_lexer_string_with_escaped_quotes() {
        let input = r#"
        str = "hello ""world"""
    "#
        .trim();
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .into_iter()
            .filter(|t| t.kind != T![ws])
            .collect();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [T![ident], T![=], T![string_literal], T![EOF],]
        );
    }

    #[test]
    fn tokenize_all_operators() {
        // we can't make a difference between unary negation and subtraction at this level
        let input = "^ * / \\ Mod + - & = <> < > <= =< >= => Is Not And Or Xor Eqv Imp";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).filter(|tk| tk != &T![ws]).collect();
        assert_eq!(
            tokens,
            vec![
                T![^],
                T![*],
                T![/],
                T!['\\'],
                T![mod],
                T![+],
                T![-],
                T![&],
                T![=],
                T![<>],
                T![<],
                T![>],
                T![<=],
                T![<=],
                T![>=],
                T![>=],
                T![is],
                T![not],
                T![and],
                T![or],
                T![xor],
                T![eqv],
                T![imp],
                T![EOF],
            ]
        );
    }

    #[test]
    fn hex_integer_literal() {
        let input = "&H10";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(tokens, vec![T![hex_integer_literal], T![EOF],]);
    }

    #[test]
    fn octal_integer_literal() {
        let input = "&O10";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(tokens, vec![T![octal_integer_literal], T![EOF],]);
    }

    #[test]
    fn double_science_notation() {
        let input = "1.401298E-45";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(tokens, vec![T![real_literal], T![EOF],]);
    }

    #[test]
    fn multi_level_property_access() {
        let input = "obj.prop1.prop2.prop3";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(
            tokens,
            vec![
                T![ident],
                T![.],
                T![ident],
                T![.],
                T![ident],
                T![.],
                T![ident],
                T![EOF],
            ]
        );
    }

    #[test]
    fn keyword_part_of_identifier() {
        let input = indoc! {r#"
        ' Stop comment
        Sub stop_sequencer()
            StopSound("metalrolling")
        End Sub"#};
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).filter(|t| t != &T![ws]).collect();
        assert_eq!(
            tokens,
            [
                T![comment],
                T![nl],
                T![sub],
                T![ident],
                T!['('],
                T![')'],
                T![nl],
                T![ident],
                T!['('],
                T![string_literal],
                T![')'],
                T![nl],
                T![end],
                T![sub],
                T![EOF],
            ]
        );
    }

    #[test]
    fn error_handling() {
        let input = indoc! {r#"
        on error resume next
        On Error Goto 0
        "#};
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).filter(|tk| tk != &T![ws]).collect();
        assert_eq!(
            tokens,
            vec![
                T![on],
                T![error],
                T![resume],
                T![next],
                T![nl],
                T![on],
                T![error],
                T![goto],
                T![integer_literal],
                T![nl],
                T![EOF],
            ]
        );
    }

    #[test]
    fn test_lexer_continuation_character() {
        let input = indoc! {r#"
        a = 1 _
            + 2 _
            + 3
        "#};
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens
            .iter()
            .map(|t| t.kind)
            .filter(|tk| tk != &T![ws])
            .collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![ident],
                T![=],
                T![integer_literal],
                T![line_continuation],
                T![+],
                T![integer_literal],
                T![line_continuation],
                T![+],
                T![integer_literal],
                T![nl],
                T![EOF],
            ]
        );
    }

    #[test]
    fn parse_two_consts_with_comments() {
        let input = indoc! {"
            Const x = 42 ' The answer to everything
            Const y = 13 ' An unlucky number
        "};
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
                T![const],
                T![ident],
                T![=],
                T![integer_literal],
                T![comment],
                T![nl],
                T![EOF],
            ]
        );
    }

    #[test]
    fn test_line_continuations_crlf() {
        let input = "x = txt & _\r\ntxt2";
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
                T![ident],
                T![=],
                T![ident],
                T![&],
                T![line_continuation],
                T![ident],
                T![EOF],
            ]
        );
    }

    #[test]
    fn test_line_continuations() {
        let input = " _ \t \r\n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![ws], T![line_continuation], T![EOF],]);
        let input = " _  \n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![ws], T![line_continuation], T![EOF],]);
        let input = " _\r";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![ws], T![line_continuation], T![EOF],]);
        let input = "this &_\r\nthat";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(
            tokens,
            [
                T![ident],
                T![ws],
                T![&],
                T![line_continuation],
                T![ident],
                T![EOF],
            ]
        );
    }

    #[test]
    fn test_newlines() {
        // CRLF should get processed as a single newline
        let input = "\r\n\n\r";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![nl], T![nl], T![nl], T![EOF],]);
    }

    #[test]
    fn test_identifier_cant_start_with_underscore() {
        let input = "_x";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_eq!(
            tokens,
            [
                Token::error(0..1),
                Token::ident(1..2, 0, 1),
                Token::eof(0..0)
            ]
        );
    }

    #[test]
    fn test_identifier_can_end_with_underscore() {
        let input = "x_";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![ident], T![EOF],]);
    }

    #[test]
    fn float_literal_without_fraction() {
        let input = "1.";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![real_literal], T![EOF],]);
    }

    #[test]
    fn float_literal_without_integral() {
        let input = ".1";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![real_literal], T![EOF],]);
    }

    #[test]
    fn tokenize_spaced_property_access() {
        // This should even work with _ between the . and the property name
        let input = "o. _\n s";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![ident],
                T![.],
                T![ws],
                T![line_continuation],
                T![ws],
                T![ident],
                T![EOF],
            ]
        );
    }

    #[test]
    fn tokenize_spaced_property_access_invalid() {
        // Fails on windows with: runtime error: Invalid or unqualified reference
        let input = "o .s";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![ident], T![ws], T![.], T![ident], T![EOF],]);
    }
}
