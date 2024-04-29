use indoc::indoc;
use pretty_assertions::assert_eq;

use vbsfmt::{lexer::*, T};

/// walks `$tokens` and compares them to the given kinds.
macro_rules! assert_tokens {
    ($tokens:ident, [$($kind:expr,)*]) => {
        {
            let mut it = $tokens.iter();
            $(
                let token = it.next().expect("not enough tokens");
            assert_eq!(token.kind, $kind);
        )*
        }
    };
}

#[test]
fn single_char_tokens() {
    let input = "+-(.):";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_tokens!(
        tokens,
        [T![+], T![-], T!['('], T![.], T![')'], T![:], T![EOF],]
    );
}

#[test]
fn unknown_input() {
    let input = "{$$$$$$$+";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_tokens!(tokens, [T!['{'], T![error], T![+], T![EOF],]);
}

#[test]
fn token_spans() {
    {
        let input = "+-(.):";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let dot = tokens[3];
        assert_eq!(dot.kind, T![.]);
        assert_eq!(dot.span, (3..4).into())
    }
    {
        let input = "{$$$$$$$+";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let error = tokens[1];
        assert_eq!(error.kind, T![error]);
        assert_eq!(error.span, (1..8).into())
    }
}

#[test]
fn single_char_tokens_with_whitespace() {
    let input = "   + -  (.): ";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    let leading_space = &tokens[0];
    assert_eq!(leading_space.kind, T![ws]);
    assert_eq!(leading_space.len(), 3);

    let space_after_minus = &tokens[4];
    assert_eq!(space_after_minus.kind, T![ws]);
    assert_eq!(space_after_minus.len(), 2);

    let trailing_space = &tokens[9];
    assert_eq!(trailing_space.kind, T![ws]);
    assert_eq!(trailing_space.len(), 1);

    let tokens: Vec<_> = tokens.into_iter().filter(|t| t.kind != T![ws]).collect();
    assert_tokens!(
        tokens,
        [T![+], T![-], T!['('], T![.], T![')'], T![:], T![EOF],]
    );
}

#[test]
fn test_lexer_only_whitespace() {
    let input = "  \t ";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_tokens!(tokens, [T![ws], T![EOF],]);
}

#[test]
fn maybe_multiple_char_tokens() {
    let input = "&&=<=_!=||";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_tokens!(
        tokens,
        [T![&&], T![=], T![<=], T![_], T![!=], T![||], T![EOF],]
    );
}

#[test]
fn keywords() {
    let input = "if dim = function else sub";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [
            T![if],
            T![dim],
            T![=],
            T![function],
            T![else],
            T![sub],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_function() {
    let input = indoc! { r#"
        'tests stuff
        Function add(a, b)
	        test = a + b
        End Function
    "#};
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
            // comment
            T![comment],
            T![nl],
            // function signature
            T![function],
            T![ident],
            T!['('],
            T![ident],
            T![,],
            T![ident],
            T![')'],
            T![nl],
            // function result assignment
            T![ident],
            T![=],
            T![ident],
            T![+],
            T![ident],
            T![nl],
            // end function
            T![end],
            T![function],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_sub() {
    let input = indoc! {r#"
        Sub sw35_Hit() 'Drain
            UpdateTrough
            Controller.Switch(35) = 1
            RandomSoundDrain sw35
        End Sub
    "#};
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
    assert_eq!(
        //assert_tokens!(
        token_kinds,
        [
            // sub
            T![sub],
            //T![ws],
            T![ident],
            T!['('],
            T![')'],
            T![comment],
            T![nl],
            // UpdateTrough
            T![ident],
            T![nl],
            T![ident],
            T![.],
            T![ident],
            T!['('],
            T![int],
            T![')'],
            T![=],
            T![int],
            T![nl],
            // RandomSoundDrain
            T![ident],
            T![ident],
            T![nl],
            // end sub
            T![end],
            T![sub],
            T![nl],
            T![EOF],
        ]
    );
    let update_through = tokens[6];
    assert_eq!("UpdateTrough", &input[update_through.span]);
}

#[test]
fn test_lexer_if_else() {
    let input = indoc! {r#"
        If (a = b) Then
            a = 1
        Else
            a = 2
        End If
    "#};
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
            T![if],
            T!['('],
            T![ident],
            T![=],
            T![ident],
            T![')'],
            T![then],
            T![nl],
            T![ident],
            T![=],
            T![int],
            T![nl],
            T![else],
            T![nl],
            T![ident],
            T![=],
            T![int],
            T![nl],
            T![end],
            T![if],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_string() {
    let input = r#""Hello, World!""#;
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(tokens, [T![string], T![EOF],]);
}

#[test]
fn test_lexer_array_declaration() {
    let input = "Dim a(1, 2, 3)";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [
            T![dim],
            T![ident],
            T!['('],
            T![int],
            T![,],
            T![int],
            T![,],
            T![int],
            T![')'],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_colon_separator() {
    let input = r#"Dim a: a = "Hello, World!""#;
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [
            T![dim],
            T![ident],
            T![:],
            T![ident],
            T![=],
            T![string],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_class() {
    let input = indoc! {r#"
        Class MyClass
            Dim a
            Sub MySub()
                a = 1
            End Sub
        End Class
    "#};
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
            T![class],
            T![ident],
            T![nl],
            T![dim],
            T![ident],
            T![nl],
            T![sub],
            T![ident],
            T!['('],
            T![')'],
            T![nl],
            T![ident],
            T![=],
            T![int],
            T![nl],
            T![end],
            T![sub],
            T![nl],
            T![end],
            T![class],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_single_line_multi_const() {
    let input = "Const a = 1, b = 2, c = 3";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [
            T![const],
            T![ident],
            T![=],
            T![int],
            T![,],
            T![ident],
            T![=],
            T![int],
            T![,],
            T![ident],
            T![=],
            T![int],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_for() {
    let input = indoc! {r#"
        For i = 1 To 10 Step 2
            Debug.Print i
        Next i
    "#};
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
            T![for],
            T![ident],
            T![=],
            T![int],
            T![to],
            T![int],
            T![step],
            T![int],
            T![nl],
            T![ident],
            T![.],
            T![ident],
            T![ident],
            T![nl],
            T![next],
            T![ident],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_while() {
    let input = indoc! {r#"
        Dim x:x=1
        Do While x<5
            document.write("Welcome.")
            x=x+1
        Loop
    "#};
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
            T![dim],
            T![ident],
            T![:],
            T![ident],
            T![=],
            T![int],
            T![nl],
            T![do],
            T![while],
            T![ident],
            T![<],
            T![int],
            T![nl],
            T![ident],
            T![.],
            T![ident],
            T!['('],
            T![string],
            T![')'],
            T![nl],
            T![ident],
            T![=],
            T![ident],
            T![+],
            T![int],
            T![nl],
            T![loop],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_select_case() {
    let input = indoc! {r#"
        select case strPerson
           case "Alex"
              WScript.Echo "We found Alex"
           case "Jasper"
              WScript.Echo "We found Jasper"
           case else
              WScript.Echo "We found someone else"   
        end select 
    "#};
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
            T![select],
            T![case],
            T![ident],
            T![nl],
            T![case],
            T![string],
            T![nl],
            T![ident],
            T![.],
            T![ident],
            T![string],
            T![nl],
            T![case],
            T![string],
            T![nl],
            T![ident],
            T![.],
            T![ident],
            T![string],
            T![nl],
            T![case],
            T![else],
            T![nl],
            T![ident],
            T![.],
            T![ident],
            T![string],
            T![nl],
            T![end],
            T![select],
            T![nl],
            T![EOF],
        ]
    );
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