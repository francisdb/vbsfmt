use super::TokenKind;
use crate::T;
use logos::{Lexer, Logos};

/* ANCHOR: callbacks */
/// Update the line count and the char index.
fn newline_callback(lex: &mut Lexer<LogosToken>) -> (usize, usize) {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
    (lex.extras.0, lex.extras.1)
}

/// Compute the line and column position for the current word.
fn word_callback(lex: &mut Lexer<LogosToken>) -> (usize, usize) {
    let line = lex.extras.0;
    let column = lex.span().start - lex.extras.1;

    (line, column)
}
/* ANCHOR_END: callbacks */

#[derive(Logos, Debug, PartialEq, Eq)]
#[logos(extras = (usize, usize))]
pub(super) enum LogosToken {
    #[token(":", word_callback)]
    Colon((usize, usize)),
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Slash,
    #[token("\\")]
    BackSlash,
    #[token("^")]
    Pow,
    #[token("=")]
    Eq,
    #[token("<>")]
    Neq,
    #[regex(r#"<=|=<"#)]
    Leq,
    #[regex(r#">=|=>"#)]
    Geq,
    #[token("&", word_callback)]
    Ampersand((usize, usize)),
    // Brackets
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    // Constructs
    #[regex(r#""([^"]|"")*""#)]
    String,
    #[regex(r#"\d+"#, priority = 2)]
    Int,
    #[regex(r#"&H[0-9A-Fa-f]+"#, priority = 2)]
    HexInt,
    #[regex(r#"&O[0-7]+"#, priority = 2)]
    OctalInt,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    Float,
    #[regex(r#"([A-Za-z]|_)([A-Za-z]|_|\d)*"#, word_callback, priority = 3)]
    Ident((usize, usize)),

    // Keywords
    #[token("and", ignore(ascii_case))]
    KwAnd,
    #[token("byref", ignore(ascii_case))]
    KwByRef,
    #[token("byval", ignore(ascii_case))]
    KwByVal,
    #[token("call", ignore(ascii_case))]
    KwCall,
    #[token("case", ignore(ascii_case))]
    KwCase,
    #[token("class", ignore(ascii_case))]
    KwClass,
    #[token("const", ignore(ascii_case))]
    KwConst,
    #[token("currency", ignore(ascii_case))]
    KwCurrency,
    // #[token("debug", ignore(ascii_case))]
    // KwDebug,
    #[token("dim", ignore(ascii_case))]
    KwDim,
    #[token("do", ignore(ascii_case))]
    KwDo,
    #[token("each", ignore(ascii_case))]
    KwEach,
    #[token("else", ignore(ascii_case))]
    KwElse,
    #[token("elseif", ignore(ascii_case))]
    KwElseIf,
    #[token("empty", word_callback, ignore(ascii_case))]
    KwEmpty((usize, usize)),
    #[token("end", ignore(ascii_case))]
    KwEnd,
    #[token("eqv", ignore(ascii_case))]
    KwEqv,
    #[token("error", ignore(ascii_case))]
    KwError,
    #[token("event", ignore(ascii_case))]
    KwEvent,
    #[token("exit", ignore(ascii_case))]
    KwExit,
    #[token("false", ignore(ascii_case))]
    KwFalse,
    #[token("for", ignore(ascii_case))]
    KwFor,
    #[token("function", ignore(ascii_case))]
    KwFunction,
    #[token("get", ignore(ascii_case))]
    KwGet,
    #[token("goto", ignore(ascii_case))]
    KwGoTo,
    #[token("if", ignore(ascii_case))]
    KwIf,
    #[token("imp", ignore(ascii_case))]
    KwImp,
    #[token("implements", ignore(ascii_case))]
    KwImplements,
    #[token("in", ignore(ascii_case))]
    KwIn,
    #[token("is", word_callback, ignore(ascii_case))]
    KwIs((usize, usize)),
    #[token("let", ignore(ascii_case))]
    KwLet,
    #[token("like", ignore(ascii_case))]
    KwLike,
    #[token("loop", ignore(ascii_case))]
    KwLoop,
    #[token("lset", ignore(ascii_case))]
    KwLSet,
    #[token("me", ignore(ascii_case))]
    KwMe,
    #[token("mod", word_callback, ignore(ascii_case))]
    KwMod((usize, usize)),
    #[token("new", ignore(ascii_case))]
    KwNew,
    #[token("next", ignore(ascii_case))]
    KwNext,
    #[token("not", ignore(ascii_case))]
    KwNot,
    #[token("nothing", ignore(ascii_case))]
    KwNothing,
    #[token("null", ignore(ascii_case))]
    KwNull,
    #[token("on", ignore(ascii_case))]
    KwOn,
    #[token("option", ignore(ascii_case))]
    KwOption,
    #[token("optional", ignore(ascii_case))]
    KwOptional,
    #[token("or", ignore(ascii_case))]
    KwOr,
    #[token("paramarray", ignore(ascii_case))]
    KwParamArray,
    #[token("private", ignore(ascii_case))]
    KwPrivate,
    #[token("property", ignore(ascii_case))]
    KwProperty,
    #[token("public", ignore(ascii_case))]
    KwPublic,
    #[token("raiseevent", ignore(ascii_case))]
    KwRaiseEvent,
    #[token("redim", ignore(ascii_case))]
    KwReDim,
    // #[token("rem", ignore(ascii_case))]
    // KwRem,
    #[token("resume", ignore(ascii_case))]
    KwResume,
    #[token("rset", ignore(ascii_case))]
    KwRSet,
    #[token("select", word_callback, ignore(ascii_case))]
    KwSelect((usize, usize)),
    #[token("set", ignore(ascii_case))]
    KwSet,
    #[token("shared", ignore(ascii_case))]
    KwShared,
    #[token("single", ignore(ascii_case))]
    KwSingle,
    #[token("static", ignore(ascii_case))]
    KwStatic,
    // In the listing I found 'step' was missing as keyword so I wonder if this
    // should be handled in a different way.
    #[token("step", ignore(ascii_case))]
    KwStep,
    #[token("sub", word_callback, ignore(ascii_case))]
    KwSub((usize, usize)),
    #[token("then", ignore(ascii_case))]
    KwThen,
    #[token("to", ignore(ascii_case))]
    KwTo,
    #[token("true", ignore(ascii_case))]
    KwTrue,
    #[token("type", ignore(ascii_case))]
    KwType,
    #[token("typeof", ignore(ascii_case))]
    KwTypeOf,
    #[token("until", ignore(ascii_case))]
    KwUntil,
    #[token("variant", ignore(ascii_case))]
    KwVariant,
    #[token("wend", ignore(ascii_case))]
    KwWend,
    #[token("while", ignore(ascii_case))]
    KwWhile,
    #[token("with", ignore(ascii_case))]
    KwWith,
    #[token("xor", ignore(ascii_case))]
    KwXor,
    /// Represents reserved keywords but that are not actually in use
    /// https://isvbscriptdead.com/reserved-keywords/
    // As, Byte, Boolean, Double, Integer, Long, Single, Stop, Variant
    #[regex(r"(?i)as|byte|boolean|double|integer|long|single|stop|variant")]
    KwUnused,

    #[regex(r"\.([A-Za-z]|_)([A-Za-z]|_|\d)*", word_callback)]
    PropertyAccess((usize, usize)),

    // Misc
    #[regex(r"[ \t\f]+")]
    WS,
    #[regex(r"\r\n|\n|\r", newline_callback)]
    NewLine((usize, usize)),
    #[regex(r" _[\r\n|\n|\r]")]
    LineContinuation,

    // comments using ' or REM
    #[regex(r"'([^\r\n]*)")]
    Comment,
    // #[error]
    // Error,
}

impl LogosToken {
    pub fn line_column(&self) -> (usize, usize) {
        use LogosToken::*;
        let mut line_col = match self {
            Ampersand((line, column)) => (*line, *column),
            Colon((line, column)) => (*line, *column),
            Ident((line, column)) => (*line, *column),
            KwEmpty((line, column)) => (*line, *column),
            KwMod((line, column)) => (*line, *column),
            KwSelect((line, column)) => (*line, *column),
            KwSub((line, column)) => (*line, *column),
            KwIs((line, column)) => (*line, *column),
            NewLine((line, _)) => (*line, 0),
            PropertyAccess((line, column)) => (*line, *column),
            _ => (0, 0),
        };
        // further down lines are 1-indexed
        line_col.0 += 1;
        line_col
    }

    #[rustfmt::skip]
    pub fn kind(&self) -> TokenKind {
        use LogosToken::*;
        match self {
            Colon(_)        => T![:],
            Comma        => T![,],
            Semi         => T![;],
            Plus         => T![+],
            Minus        => T![-],
            Times        => T![*],
            Slash        => T![/],
            BackSlash    => T!['\\'],
            Pow          => T![^],
            Eq           => T![=],
            Neq          => T![<>],
            Leq          => T![<=],
            Geq          => T![>=],
            LAngle       => T![<],
            RAngle       => T![>],
            Ampersand(_)    => T![&],
            LParen       => T!['('],
            RParen       => T![')'],
            String       => T![string_literal],
            Int          => T![integer_literal],
            HexInt       => T![hex_integer_literal],
            OctalInt     => T![octal_integer_literal],
            Float        => T![real_literal],
            Ident(_)     => T![ident],
            KwAnd        => T![and],
            KwByRef      => T![byref],
            KwByVal      => T![byval],
            KwCall       => T![call],
            KwCase       => T![case],
            KwClass      => T![class],
            KwConst      => T![const],
            KwCurrency   => unimplemented!("KwCurrency"),
            KwDim        => T![dim],
            KwDo         => T![do],
            KwEach       => T![each],
            KwElse       => T![else],
            KwElseIf     => T![elseif],
            KwEmpty(_)   => T![empty],
            KwEnd        => T![end],
            KwEqv        => T![eqv],
            KwError      => T![error],
            KwEvent      => unimplemented!("KwEvent"),
            KwExit       => T![exit],
            KwFalse      => T![false],
            KwFor        => T![for],
            KwFunction   => T![function],
            KwGet        => T![get],
            KwGoTo       => T![goto],
            KwIf         => T![if],
            KwImp        => T![imp],
            KwImplements => unimplemented!("KwImplements"),
            KwIn         => T![in],
            KwIs(_)      => T![is],
            KwLet        => T![let],
            KwLike       => unimplemented!( "KwLike"),
            KwLoop       => T![loop],
            KwLSet       => unimplemented!( "KwLSet"),
            KwMe         => T![me],
            KwMod(_)     => T![mod],
            KwNew        => T![new],
            KwNext       => T![next],
            KwNot        => T![not],
            KwNothing    => T![nothing],
            KwNull       => T![null],
            KwOn         => T![on],
            KwOption     => T![option],
            KwOptional   => unimplemented!(),
            KwOr         => T![or],
            KwParamArray => unimplemented!( "KwParamArray"),
            KwPrivate    => T![private],
            KwProperty   => T![property],
            KwPublic     => T![public],
            KwRaiseEvent => unimplemented!( "KwRaiseEvent"),
            KwReDim      => T![redim],
            KwResume     => T![resume],
            KwRSet       => unimplemented!( "KwRSet"),
            KwSelect(_)  => T![select],
            KwSet        => T![set],
            KwShared     => unimplemented!( "KwShared"),
            KwSingle     => unimplemented!( "KwSingle"),
            KwStatic     => unimplemented!( "KwStatic"),
            KwStep       => T![step],
            KwSub(_)     => T![sub],
            KwThen       => T![then],
            KwTo         => T![to],
            KwTrue       => T![true],
            KwType       => unimplemented!("KwType"),
            KwTypeOf     => unimplemented!( "KwTypeOf"),
            KwUntil      => T![until],
            KwVariant    => unimplemented!( "KwVariant"),
            KwWend       => T![wend],
            KwWhile      => T![while],
            KwWith       => T![with],
            KwXor        => T![xor],
            KwUnused     => T![unused],
            PropertyAccess(_) => T![property_access],
            WS           => T![ws],
            Comment      => T![comment],
            NewLine(_)   => T![nl],
            LineContinuation => T![line_continuation],
        }
    }
}

#[cfg(test)]
mod test {
    use super::LogosToken;
    use crate::T;
    use logos::Logos;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_lexer() {
        let input = "Dim x:x = 42\n";
        let lexer = LogosToken::lexer(input);
        let token_kinds = lexer
            .spanned()
            .map(|(res, span)| match res {
                Ok(t) => t.kind(),
                Err(_e) => {
                    let section = &input[span.start..span.end];
                    panic!(
                        "Some error occurred between char {} and {}: {}",
                        span.start, span.end, section
                    )
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![dim],
                T![ws],
                T![ident],
                T![:],
                T![ident],
                T![ws],
                T![=],
                T![ws],
                T![integer_literal],
                T![nl]
            ]
        );
    }
}
