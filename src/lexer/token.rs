use std::fmt;
use std::ops::{Index, Range};

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} - <{}, {}>",
            self.kind, self.span.start, self.span.end
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Default, Debug)]
pub struct Span {
    /// inclusive
    pub start: u32,
    /// exclusive
    pub end: u32,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TokenKind {
    // Single characters
    Plus,
    Minus,
    Times,
    Slash,
    Pow,
    Eq,
    Dot,
    Comma,
    Underscore,
    Bang,
    Ampersand,
    Bar,
    Colon,
    SemiColon,
    // Brackets
    LAngle,
    RAngle,
    LSquare,
    RSquare,
    LBrace,
    RBrace,
    LParen,
    RParen,
    // Multiple characters
    Option,
    String,
    Comment,
    Int,
    Float,
    Identifier,
    KeywordConst,
    KeywordDim,
    KeywordRedim,
    KeywordSub,
    KeywordFunction,
    KeywordIf,
    KeywordThen,
    KeywordElse,
    KeywordSet,
    KeywordEnd,
    KeywordClass,
    KeywordProperty,
    KeywordPublic,
    KeywordPrivate,
    KeywordAs,
    KeywordNew,
    KeywordReturn,
    KeywordFor,
    KeywordEach,
    KeywordIn,
    KeywordTo,
    KeywordStep,
    KeywordNext,
    KeywordWhile,
    KeywordWend,
    KeywordDo,
    KeywordLoop,
    KeywordUntil,
    KeywordWith,
    KeywordSelect,
    KeywordCase,
    KeywordCall,
    KeywordExit,
    //Special values
    Empty,
    Null,
    Nothing,
    True,
    False,
    // Operators
    And,
    Or,
    Eqq,
    Neq,
    Geq,
    Leq,
    // Misc,
    Error,
    Whitespace,
    /// CRLF, LF (or CR)
    Newline,
    Eof,
}

#[macro_export]
macro_rules! T {
    [+] => {
        $crate::lexer::TokenKind::Plus
    };
    [-] => {
        $crate::lexer::TokenKind::Minus
    };
    [*] => {
        $crate::lexer::TokenKind::Times
    };
    [/] => {
        $crate::lexer::TokenKind::Slash
    };
    [^] => {
        $crate::lexer::TokenKind::Pow
    };
    [=] => {
        $crate::lexer::TokenKind::Eq
    };
    [.] => {
        $crate::lexer::TokenKind::Dot
    };
    [,] => {
        $crate::lexer::TokenKind::Comma
    };
    [_] => {
        $crate::lexer::TokenKind::Underscore
    };
    [!] => {
        $crate::lexer::TokenKind::Bang
    };
    [&] => {
        $crate::lexer::TokenKind::Ampersand
    };
    [|] => {
        $crate::lexer::TokenKind::Bar
    };
    [:] => {
        $crate::lexer::TokenKind::Colon
    };
    [;] => {
        $crate::lexer::TokenKind::SemiColon
    };
    [<] => {
        $crate::lexer::TokenKind::LAngle
    };
    [>] => {
        $crate::lexer::TokenKind::RAngle
    };
    ['['] => {
        $crate::lexer::TokenKind::LSquare
    };
    [']'] => {
        $crate::lexer::TokenKind::RSquare
    };
    ['{'] => {
        $crate::lexer::TokenKind::LBrace
    };
    ['}'] => {
        $crate::lexer::TokenKind::RBrace
    };
    ['('] => {
        $crate::lexer::TokenKind::LParen
    };
    [')'] => {
        $crate::lexer::TokenKind::RParen
    };
    [option] => {
        $crate::lexer::TokenKind::Option
    };
    [string] => {
        $crate::lexer::TokenKind::String
    };
    [comment] => {
        $crate::lexer::TokenKind::Comment
    };
    [int] => {
        $crate::lexer::TokenKind::Int
    };
    [float] => {
        $crate::lexer::TokenKind::Float
    };
    [ident] => {
        $crate::lexer::TokenKind::Identifier
    };
    [const] => {
        $crate::lexer::TokenKind::KeywordConst
    };
    [dim] => {
        $crate::lexer::TokenKind::KeywordDim
    };
    [redim] => {
        $crate::lexer::TokenKind::KeywordRedim
    };
    [set] => {
        $crate::lexer::TokenKind::KeywordSet
    };
    [sub] => {
        $crate::lexer::TokenKind::KeywordSub
    };
    [function] => {
        $crate::lexer::TokenKind::KeywordFunction
    };
    [call] => {
        $crate::lexer::TokenKind::KeywordCall
    };
    [class] => {
        $crate::lexer::TokenKind::KeywordClass
    };
    [property] => {
        $crate::lexer::TokenKind::KeywordProperty
    };
    [public] => {
        $crate::lexer::TokenKind::KeywordPublic
    };
    [private] => {
        $crate::lexer::TokenKind::KeywordPrivate
    };
    [as] => {
        $crate::lexer::TokenKind::KeywordAs
    };
    [new] => {
        $crate::lexer::TokenKind::KeywordNew
    };
    [return] => {
        $crate::lexer::TokenKind::KeywordReturn
    };
    [for] => {
        $crate::lexer::TokenKind::KeywordFor
    };
    [each] => {
        $crate::lexer::TokenKind::KeywordEach
    };
    [in] => {
        $crate::lexer::TokenKind::KeywordIn
    };
    [to] => {
        $crate::lexer::TokenKind::KeywordTo
    };
    [step] => {
        $crate::lexer::TokenKind::KeywordStep
    };
    [next] => {
        $crate::lexer::TokenKind::KeywordNext
    };
    [while] => {
        $crate::lexer::TokenKind::KeywordWhile
    };
    [wend] => {
        $crate::lexer::TokenKind::KeywordWend
    };
    [do] => {
        $crate::lexer::TokenKind::KeywordDo
    };
    [loop] => {
        $crate::lexer::TokenKind::KeywordLoop
    };
    [until] => {
        $crate::lexer::TokenKind::KeywordUntil
    };
    [with] => {
        $crate::lexer::TokenKind::KeywordWith
    };
    [select] => {
        $crate::lexer::TokenKind::KeywordSelect
    };
    [case] => {
        $crate::lexer::TokenKind::KeywordCase
    };
    [if] => {
        $crate::lexer::TokenKind::KeywordIf
    };
    [then] => {
        $crate::lexer::TokenKind::KeywordThen
    };
    [else] => {
        $crate::lexer::TokenKind::KeywordElse
    };
    [end] => {
        $crate::lexer::TokenKind::KeywordEnd
    };
    [exit] => {
        $crate::lexer::TokenKind::KeywordExit
    };
    // Special values
    [empty] => {
        $crate::lexer::TokenKind::Empty
    };
    [null] => {
        $crate::lexer::TokenKind::Null
    };
    [nothing] => {
        $crate::lexer::TokenKind::Nothing
    };
    [true] => {
        $crate::lexer::TokenKind::True
    };
    [false] => {
        $crate::lexer::TokenKind::False
    };
    [&&] => {
        $crate::lexer::TokenKind::And
    };
    [||] => {
        $crate::lexer::TokenKind::Or
    };
    [==] => {
        $crate::lexer::TokenKind::Eqq
    };
    [!=] => {
        $crate::lexer::TokenKind::Neq
    };
    [>=] => {
        $crate::lexer::TokenKind::Geq
    };
    [<=] => {
        $crate::lexer::TokenKind::Leq
    };
    [error] => {
        $crate::lexer::TokenKind::Error
    };
    [ws] => {
        $crate::lexer::TokenKind::Whitespace
    };
    [nl] => {
        $crate::lexer::TokenKind::Newline
    };
    [EOF] => {
        $crate::lexer::TokenKind::Eof
    };
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // Single characters
                T![+] => "+",
                T![-] => "-",
                T![*] => "*",
                T![/] => "/",
                T![^] => "^",
                T![=] => "=",
                T![.] => ".",
                T![,] => ",",
                T![_] => "_",
                T![!] => "!",
                T![&] => "&",
                T![|] => "|",
                T![:] => ":",
                T![;] => ";",
                // Brackets
                T![<] => "<",
                T![>] => ">",
                T!['['] => "[",
                T![']'] => "]",
                T!['{'] => "{",
                T!['}'] => "}",
                T!['('] => "(",
                T![')'] => ")",
                // Multiple characters
                T![option] => "option",
                T![string] => "string",
                T![comment] => "// comment",
                T![int] => "int",
                T![float] => "float",
                T![ident] => "identifier",
                T![const] => "const",
                T![dim] => "dim",
                T![redim] => "redim",
                T![set] => "set",
                T![sub] => "sub",
                T![function] => "function",
                T![class] => "class",
                T![property] => "property",
                T![public] => "public",
                T![private] => "private",
                T![call] => "call",
                T![as] => "as",
                T![new] => "new",
                T![return] => "return",
                T![for] => "for",
                T![each] => "each",
                T![in] => "in",
                T![to] => "to",
                T![step] => "step",
                T![next] => "next",
                T![while] => "while",
                T![wend] => "wend",
                T![do] => "do",
                T![loop] => "loop",
                T![until] => "until",
                T![with] => "with",
                T![select] => "select",
                T![case] => "case",
                T![if] => "if",
                T![then] => "then",
                T![else] => "else",
                T![end] => "end",
                T![exit] => "exit",
                // Special values
                T![empty] => "empty",
                T![null] => "null",
                T![nothing] => "nothing",
                T![true] => "true",
                T![false] => "false",
                // Operators
                T![&&] => "&&",
                T![||] => "||",
                T![==] => "==",
                T![!=] => "!=",
                T![>=] => ">=",
                T![<=] => "<=",
                // Misc
                T![error] => "<?>",
                T![ws] => "<WS>",
                T![nl] => "<NL>",
                T![EOF] => "<EOF>",
            }
        )
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn token_kind_display() {
        assert_eq!(T![+].to_string(), "+");
        assert_eq!(T![<=].to_string(), "<=");
        assert_eq!(T![dim].to_string(), "dim");
        assert_eq!(T![error].to_string(), "<?>");
        assert_eq!(T![comment].to_string(), "// comment");
    }
}
