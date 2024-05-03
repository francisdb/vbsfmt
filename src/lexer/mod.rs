use crate::lexer::generated::LogosToken;
use logos::Logos;
pub use token::{Token, TokenKind};

use crate::lexer::rules::{unambiguous_single_char, Rule};
use crate::lexer::token::Span;
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
                Ok(token) => Some(Token {
                    kind: token.kind(),
                    span: span.into(),
                }),
                Err(_) => Some(Token {
                    kind: T![error],
                    span: span.into(),
                }),
            },
            None if self.eof => None,
            None => {
                self.eof = true;
                Some(Token {
                    kind: T![EOF],
                    span: (0..0).into(),
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
            })
        } else {
            Some(self.next_token(&self.input[self.position as usize..]))
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
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
                T![property_access],
                T![property_access],
                T![property_access],
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
}
