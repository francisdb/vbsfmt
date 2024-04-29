pub use token::TokenKind;

use crate::lexer::rules::{Rule, unambiguous_single_char};
use crate::lexer::token::{Span, Token};
use crate::T;

mod rules;
mod token;

// pub type Lexer<'input> = CustomLexer<'input>;
// //pub type Lexer<'input> = LogosLexer<'input>;

pub struct Lexer<'input> {
    input: &'input str,
    position: u32,
    eof: bool,
    rules: Vec<Rule>,
}

impl<'input> Lexer<'input> {
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

impl<'input> Iterator for Lexer<'input> {
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
}
