use super::TokenKind;
use crate::T;
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq)]
pub(super) enum LogosToken {
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
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
    #[token("^")]
    Pow,
    #[token("=")]
    Eq,
    #[token("!")]
    Or,
    #[token("==")]
    Eqq,
    #[token("<>")]
    Neq,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("_", priority = 2)]
    Under,
    // Brackets
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    // Constructs
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    String,
    #[regex(r#"\d+"#, priority = 2)]
    Int,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    Float,
    #[regex(r#"([A-Za-z]|_)([A-Za-z]|_|\d)*"#, priority = 3)]
    Ident,

    // Keywords
    #[token("and", ignore(ascii_case))]
    KwAnd,
    #[token("as", ignore(ascii_case))]
    KwAs,
    #[token("boolean", ignore(ascii_case))]
    KwBoolean,
    #[token("byref", ignore(ascii_case))]
    KwByRef,
    #[token("byte", ignore(ascii_case))]
    KwByte,
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
    #[token("debug", ignore(ascii_case))]
    KwDebug,
    #[token("dim", ignore(ascii_case))]
    KwDim,
    #[token("do", ignore(ascii_case))]
    KwDo,
    #[token("double", ignore(ascii_case))]
    KwDouble,
    #[token("each", ignore(ascii_case))]
    KwEach,
    #[token("else", ignore(ascii_case))]
    KwElse,
    #[token("elseif", ignore(ascii_case))]
    KwElseIf,
    #[token("empty", ignore(ascii_case))]
    KwEmpty,
    #[token("end", ignore(ascii_case))]
    KwEnd,
    #[token("endif", ignore(ascii_case))]
    KwEndIf,
    #[token("enum", ignore(ascii_case))]
    KwEnum,
    #[token("eqv", ignore(ascii_case))]
    KwEqv,
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
    #[token("integer", ignore(ascii_case))]
    KwInteger,
    #[token("is", ignore(ascii_case))]
    KwIs,
    #[token("let", ignore(ascii_case))]
    KwLet,
    #[token("like", ignore(ascii_case))]
    KwLike,
    #[token("long", ignore(ascii_case))]
    KwLong,
    #[token("loop", ignore(ascii_case))]
    KwLoop,
    #[token("lset", ignore(ascii_case))]
    KwLSet,
    #[token("me", ignore(ascii_case))]
    KwMe,
    #[token("mod", ignore(ascii_case))]
    KwMod,
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
    #[token("preserve", ignore(ascii_case))]
    KwPreserve,
    #[token("private", ignore(ascii_case))]
    KwPrivate,
    #[token("public", ignore(ascii_case))]
    KwPublic,
    #[token("raiseevent", ignore(ascii_case))]
    KwRaiseEvent,
    #[token("redim", ignore(ascii_case))]
    KwReDim,
    #[token("rem", ignore(ascii_case))]
    KwRem,
    #[token("resume", ignore(ascii_case))]
    KwResume,
    #[token("rset", ignore(ascii_case))]
    KwRSet,
    #[token("select", ignore(ascii_case))]
    KwSelect,
    #[token("set", ignore(ascii_case))]
    KwSet,
    #[token("shared", ignore(ascii_case))]
    KwShared,
    #[token("single", ignore(ascii_case))]
    KwSingle,
    #[token("static", ignore(ascii_case))]
    KwStatic,
    #[token("step", ignore(ascii_case))]
    KwStep,
    #[token("stop", ignore(ascii_case))]
    KwStop,
    #[token("sub", ignore(ascii_case))]
    KwSub,
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

    // Misc
    #[regex(r"[ \t\f]+")]
    WS,
    #[regex(r"[\r\n|\n|\r]")]
    NewLine,

    // comments using ' or REM
    #[regex(r"'([^\r\n]*)")]
    Comment,

    // #[error]
    // Error,
}

impl LogosToken {
    #[rustfmt::skip]
    pub fn kind(&self) -> TokenKind {
        use LogosToken::*;
        match self {
            Dot          => T![.],
            Colon        => T![:],
            Comma        => T![,],
            Semi         => T![;],
            Plus         => T![+],
            Minus        => T![-],
            Times        => T![*],
            Slash        => T![/],
            Pow          => T![^],
            Eq           => T![=],
            Neq          => T![<>],
            Leq          => T![<=],
            Geq          => T![>=],
            Under        => T![_],
            LAngle       => T![<],
            RAngle       => T![>],
            LParen       => T!['('],
            RParen       => T![')'],
            LSquare      => T!['['],
            RSquare      => T![']'],
            LBrace       => T!['{'],
            RBrace       => T!['}'],
            String       => T![string],
            Int          => T![integer_literal],
            Float        => T![real_literal],
            Ident        => T![ident],
            KwAnd        => T![and],
            KwAs         => T![as],
            KwBoolean    => T![boolean],
            KwByRef      => T![byref],
            KwByte       => T![byte],
            KwByVal      => T![byval],
            KwCall       => T![call],
            KwCase       => T![case],
            KwClass      => T![class],
            KwConst      => T![const],
            // KwCurrency   => T![currency],
            // KwDebug      => T![debug],
            KwDim        => T![dim],
            KwDo         => T![do],
            KwDouble     => T![double],
            KwEach       => T![each],
            KwElseIf     => T![elseif],
            KwEmpty      => T![empty],


            
            KwLet        => T![let],
            KwIf         => T![if],
            KwElse       => T![else],
            KwEnd        => T![end],
            KwFor        => T![for],
            KwTo         => T![to],
            KwStep       => T![step],
            KwNext       => T![next],
            KwWhile      => T![while],
            KwWend       => T![wend],
            KwFunction         => T![function],
            WS           => T![ws],
            Comment      => T![comment],
            NewLine      => T![nl],
            _            => panic!("Unknown token: {:?}", self),
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
                Err(e) => {
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
