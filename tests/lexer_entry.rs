extern crate brite;

macro_rules! test {
    ($name:ident) => {
        #[test]
        fn $name() {
            use brite::diagnostics::DiagnosticsCollection;
            use brite::parser::{Lexer, Token};
            use std::fs;
            use std::path::PathBuf;

            let mut path = PathBuf::from(file!());
            path.set_file_name(stringify!($name));
            path.set_extension("ite");

            let source = fs::read_to_string(&path)
                .unwrap()
                .replace("\\n", "\n")
                .replace("\\r", "\r")
                .replace("\\t", "\t");

            let mut diagnostics = DiagnosticsCollection::new();
            let mut lexer = Lexer::new(&mut diagnostics, &source);
            let mut tokens = Vec::new();
            while let Some(token) = lexer.next() {
                tokens.push(token);
            }
            let end_token = lexer.end().unwrap();

            path.set_extension("ite.md");
            let mut contents = String::new();
            contents.push_str(&format!("# Lexer Test: `{}`\n", stringify!($name)));
            if !diagnostics.is_empty() {
                contents.push_str("\n");
                contents.push_str("## Errors\n");
                contents.push_str(&diagnostics.markdown_list());
            }
            contents.push_str("\n");
            contents.push_str("## Tokens\n");
            contents.push_str(&Token::markdown_table(&tokens, &end_token));

            fs::write(path, contents).unwrap();

            assert_eq!(Token::source(&tokens, &end_token), source);
        }
    };
}

mod lexer;
