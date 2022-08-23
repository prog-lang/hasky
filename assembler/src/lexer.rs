use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[token(".")]
    Dot,

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,

    #[token("module")]
    Module,

    #[token("export")]
    Export,

    #[token("section")]
    Section,

    #[regex(r"-?\d+", |i| i.slice().parse())]
    Integer(i32),

    #[regex("[a-zA-Z]+[0-9]*")]
    Identifier,

    #[error]
    #[regex(r"[ \r\n\t\f]+", logos::skip)]
    Error,
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::Token;

    #[test]
    fn it_works() {
        let mut lex = Token::lexer(r"
        module main;
        export main;

        section data;
        n: -42;

        section text;
        nop;
        ret;
        ");

        assert_eq!(lex.next(), Some(Token::Module));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "main");
        assert_eq!(lex.next(), Some(Token::Semicolon));

        assert_eq!(lex.next(), Some(Token::Export));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "main");
        assert_eq!(lex.next(), Some(Token::Semicolon));

        assert_eq!(lex.next(), Some(Token::Section));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "data");
        assert_eq!(lex.next(), Some(Token::Semicolon));

        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "n");
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Integer(-42)));
        assert_eq!(lex.next(), Some(Token::Semicolon));

        assert_eq!(lex.next(), Some(Token::Section));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "text");
        assert_eq!(lex.next(), Some(Token::Semicolon));

        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "nop");
        assert_eq!(lex.next(), Some(Token::Semicolon));

        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "ret");
        assert_eq!(lex.next(), Some(Token::Semicolon));

        assert_eq!(lex.next(), None);
    }
}