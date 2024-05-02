use lazy_static::lazy_static;
use regex::Regex;

use crate::T;

use super::TokenKind;

pub(crate) struct Rule {
    pub kind: TokenKind,
    pub matches: fn(&str) -> Option<u32>,
}

/// If the given character is a character that _only_ represents a token of length 1,
/// this method returns the corresponding `TokenKind`.
/// Note that this method will return `None` for characters like `=` that may also
/// occur at the first position of longer tokens (here `==`).
pub(crate) const fn unambiguous_single_char(c: char) -> Option<TokenKind> {
    Some(match c {
        '+' => T![+],
        '-' => T![-],
        '*' => T![*],
        '/' => T![/],
        '\\' => T!['\\'],
        '^' => T![^],
        '.' => T![.],
        ',' => T![,],
        '[' => T!['['],
        ']' => T![']'],
        '{' => T!['{'],
        '}' => T!['}'],
        '(' => T!['('],
        ')' => T![')'],
        ':' => T![:],
        ';' => T![;],
        _ => return None,
    })
}

fn match_single_char(input: &str, c: char) -> Option<u32> {
    input
        .chars()
        .next()
        .and_then(|ch| if ch == c { Some(1) } else { None })
}

fn match_two_chars(input: &str, first: char, second: char) -> Option<u32> {
    if input.len() >= 2 {
        match_single_char(input, first)
            .and_then(|_| match_single_char(&input[1..], second).map(|_| 2))
    } else {
        None
    }
}

/// VBScript has case-insensitive keywords, so we need to match them case-insensitively.
fn match_keyword(input: &str, keyword: &str) -> Option<u32> {
    if input
        .chars()
        .zip(keyword.chars())
        .all(|(a, b)| a.eq_ignore_ascii_case(&b))
    {
        Some(keyword.len() as u32)
    } else {
        None
    }
}

fn match_regex(input: &str, r: &Regex) -> Option<u32> {
    r.find(input).map(|regex_match| regex_match.end() as u32)
}

lazy_static! {
    /// CRLF, LF (or CR) are all valid newline characters in VBScript.
    static ref NEWLINE_REGEX: Regex = Regex::new(r#"^(\r\n|\n|\r)"#).unwrap();
    // There is only one kind of escape sequence in VBScript, and that is the double quote.
    // eg: "hello ""world"""
    static ref STRING_REGEX: Regex = Regex::new(r#"^"([^"]|"")*""#).unwrap();
    static ref COMMENT_REGEX: Regex = Regex::new(r#"^'([^\r\n]*)"#).unwrap();
    static ref FLOAT_REGEX: Regex =
        Regex::new(r#"^((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#).unwrap();
    static ref IDENTIFIER_REGEX: Regex = Regex::new(r##"^([A-Za-z]|_)([A-Za-z]|_|\d)*"##).unwrap();
}

pub(crate) fn get_rules() -> Vec<Rule> {
    vec![
        Rule {
            kind: T![!],
            matches: |input| match_single_char(input, '!'),
        },
        Rule {
            kind: T![=],
            matches: |input| match_single_char(input, '='),
        },
        // Rule {
        //     kind: T![/],
        //     matches: |input| match_single_char(input, '/'),
        // },
        Rule {
            kind: T![_],
            matches: |input| match_single_char(input, '_'),
        },
        Rule {
            kind: T![<],
            matches: |input| match_single_char(input, '<'),
        },
        Rule {
            kind: T![>],
            matches: |input| match_single_char(input, '>'),
        },
        Rule {
            kind: T![&],
            matches: |input| match_single_char(input, '&'),
        },
        Rule {
            kind: T![<>],
            matches: |input| match_two_chars(input, '<', '>'),
        },
        Rule {
            kind: T![<>],
            matches: |input| match_two_chars(input, '>', '<'),
        },
        Rule {
            kind: T![<=],
            matches: |input| match_two_chars(input, '<', '='),
        },
        Rule {
            kind: T![<=],
            matches: |input| match_two_chars(input, '=', '<'),
        },
        Rule {
            kind: T![>=],
            matches: |input| match_two_chars(input, '>', '='),
        },
        Rule {
            kind: T![>=],
            matches: |input| match_two_chars(input, '=', '>'),
        },
        Rule {
            kind: T![mod],
            matches: |input| match_keyword(input, "mod"),
        },
        Rule {
            kind: T![is],
            matches: |input| match_keyword(input, "is"),
        },
        Rule {
            kind: T![not],
            matches: |input| match_keyword(input, "not"),
        },
        Rule {
            kind: T![and],
            matches: |input| match_keyword(input, "and"),
        },
        Rule {
            kind: T![or],
            matches: |input| match_keyword(input, "or"),
        },
        Rule {
            kind: T![xor],
            matches: |input| match_keyword(input, "xor"),
        },
        Rule {
            kind: T![eqv],
            matches: |input| match_keyword(input, "eqv"),
        },
        Rule {
            kind: T![imp],
            matches: |input| match_keyword(input, "imp"),
        },
        Rule {
            kind: T![or],
            matches: |input| match_keyword(input, "xor"),
        },
        Rule {
            kind: T![option],
            matches: |input| match_keyword(input, "option"),
        },
        Rule {
            kind: T![dim],
            matches: |input| match_keyword(input, "dim"),
        },
        Rule {
            kind: T![set],
            matches: |input| match_keyword(input, "set"),
        },
        Rule {
            kind: T![let],
            matches: |input| match_keyword(input, "let"),
        },
        Rule {
            kind: T![get],
            matches: |input| match_keyword(input, "get"),
        },
        Rule {
            kind: T![sub],
            matches: |input| match_keyword(input, "sub"),
        },
        Rule {
            kind: T![function],
            matches: |input| match_keyword(input, "function"),
        },
        Rule {
            kind: T![byval],
            matches: |input| match_keyword(input, "byval"),
        },
        Rule {
            kind: T![byref],
            matches: |input| match_keyword(input, "byref"),
        },
        Rule {
            kind: T![call],
            matches: |input| match_keyword(input, "call"),
        },
        Rule {
            kind: T![new],
            matches: |input| match_keyword(input, "new"),
        },
        Rule {
            kind: T![do],
            matches: |input| match_keyword(input, "do"),
        },
        Rule {
            kind: T![loop],
            matches: |input| match_keyword(input, "loop"),
        },
        Rule {
            kind: T![while],
            matches: |input| match_keyword(input, "while"),
        },
        Rule {
            kind: T![wend],
            matches: |input| match_keyword(input, "wend"),
        },
        Rule {
            kind: T![for],
            matches: |input| match_keyword(input, "for"),
        },
        Rule {
            kind: T![to],
            matches: |input| match_keyword(input, "to"),
        },
        Rule {
            kind: T![step],
            matches: |input| match_keyword(input, "step"),
        },
        Rule {
            kind: T![next],
            matches: |input| match_keyword(input, "next"),
        },
        Rule {
            kind: T![each],
            matches: |input| match_keyword(input, "each"),
        },
        Rule {
            kind: T![in],
            matches: |input| match_keyword(input, "in"),
        },
        Rule {
            kind: T![exit],
            matches: |input| match_keyword(input, "exit"),
        },
        Rule {
            kind: T![select],
            matches: |input| match_keyword(input, "select"),
        },
        Rule {
            kind: T![case],
            matches: |input| match_keyword(input, "case"),
        },
        Rule {
            kind: T![const],
            matches: |input| match_keyword(input, "const"),
        },
        Rule {
            kind: T![redim],
            matches: |input| match_keyword(input, "redim"),
        },
        Rule {
            kind: T![with],
            matches: |input| match_keyword(input, "with"),
        },
        Rule {
            kind: T![class],
            matches: |input| match_keyword(input, "class"),
        },
        Rule {
            kind: T![property],
            matches: |input| match_keyword(input, "property"),
        },
        Rule {
            kind: T![public],
            matches: |input| match_keyword(input, "public"),
        },
        Rule {
            kind: T![private],
            matches: |input| match_keyword(input, "private"),
        },
        Rule {
            kind: T![if],
            matches: |input| match_keyword(input, "if"),
        },
        Rule {
            kind: T![then],
            matches: |input| match_keyword(input, "then"),
        },
        Rule {
            kind: T![elseif],
            matches: |input| match_keyword(input, "elseif"),
        },
        Rule {
            kind: T![else],
            matches: |input| match_keyword(input, "else"),
        },
        Rule {
            kind: T![end],
            matches: |input| match_keyword(input, "end"),
        },
        // Data types
        Rule {
            kind: T![boolean],
            matches: |input| match_keyword(input, "boolean"),
        },
        Rule {
            kind: T![byte],
            matches: |input| match_keyword(input, "byte"),
        },
        Rule {
            kind: T![char],
            matches: |input| match_keyword(input, "char"),
        },
        Rule {
            kind: T![date],
            matches: |input| match_keyword(input, "date"),
        },
        Rule {
            kind: T![decimal],
            matches: |input| match_keyword(input, "decimal"),
        },
        Rule {
            kind: T![double],
            matches: |input| match_keyword(input, "double"),
        },
        Rule {
            kind: T![integer],
            matches: |input| match_keyword(input, "integer"),
        },
        Rule {
            kind: T![long],
            matches: |input| match_keyword(input, "long"),
        },
        Rule {
            kind: T![short],
            matches: |input| match_keyword(input, "short"),
        },
        Rule {
            kind: T![single],
            matches: |input| match_keyword(input, "single"),
        },
        Rule {
            kind: T![string],
            matches: |input| match_keyword(input, "string"),
        },
        // Special values
        Rule {
            kind: T![empty],
            matches: |input| match_keyword(input, "empty"),
        },
        Rule {
            kind: T![null],
            matches: |input| match_keyword(input, "null"),
        },
        Rule {
            kind: T![nothing],
            matches: |input| match_keyword(input, "nothing"),
        },
        Rule {
            kind: T![true],
            matches: |input| match_keyword(input, "true"),
        },
        Rule {
            kind: T![false],
            matches: |input| match_keyword(input, "false"),
        },
        // Error handling
        Rule {
            kind: T![on],
            matches: |input| match_keyword(input, "on"),
        },
        Rule {
            kind: T![error],
            matches: |input| match_keyword(input, "error"),
        },
        Rule {
            kind: T![resume],
            matches: |input| match_keyword(input, "resume"),
        },
        Rule {
            kind: T![next],
            matches: |input| match_keyword(input, "next"),
        },
        Rule {
            kind: T![goto],
            matches: |input| match_keyword(input, "goto"),
        },
        // Regular expressions
        Rule {
            kind: T![string_literal],
            matches: move |input| match_regex(input, &STRING_REGEX),
        },
        Rule {
            kind: T![comment],
            matches: move |input| match_regex(input, &COMMENT_REGEX),
        },
        Rule {
            kind: T![nl],
            matches: move |input| match_regex(input, &NEWLINE_REGEX),
        },
        Rule {
            kind: T![integer_literal],
            matches: |input| {
                input
                    .char_indices()
                    .take_while(|(_, c)| c.is_ascii_digit())
                    .last()
                    .map(|(pos, _)| pos as u32 + 1)
            },
        },
        Rule {
            kind: T![integer_literal],
            matches: |input| {
                // integers in hex notation
                // use strip_prefix instead of starts_with to avoid allocation
                input.strip_prefix("&H").map(|s| {
                    s.char_indices()
                        .take_while(|(_, c)| c.is_ascii_hexdigit())
                        .last()
                        .map(|(pos, _)| pos as u32 + 1 + 2) // +2 for the "&H"
                        .unwrap_or(2) // +2 for the "&H"
                })
            },
        },
        Rule {
            kind: T![real_literal],
            matches: |input| match_regex(input, &FLOAT_REGEX),
        },
        Rule {
            kind: T![ident],
            matches: |input| match_regex(input, &IDENTIFIER_REGEX),
        },
    ]
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_string() {
        let input = r#""hello""#;
        let m = STRING_REGEX.find(input).map(|m| m.as_str());
        assert_eq!(m, Some("\"hello\""));
    }

    #[test]
    fn test_comment() {
        let input = "' comment\n";
        let m = COMMENT_REGEX.find(input).map(|m| m.as_str());
        assert_eq!(m, Some("' comment"));
    }

    #[test]
    fn test_comment_with_pipe() {
        let input = "' |test|\n";
        let m = COMMENT_REGEX.find(input).map(|m| m.as_str());
        assert_eq!(m, Some("' |test|"));
    }
}
