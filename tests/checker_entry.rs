extern crate brite;

macro_rules! test {
    ($name:ident) => {
        #[test]
        fn $name() {
            use brite::checker::Checker;
            use brite::diagnostics::DiagnosticsCollection;
            use brite::parser::{Lexer, Parser};
            use std::fs;
            use std::path::PathBuf;

            let mut path = PathBuf::from(file!());
            path.set_file_name(stringify!($name));
            path.set_extension("ite");

            let source = fs::read_to_string(&path).unwrap();

            let mut diagnostics = DiagnosticsCollection::new();
            let lexer = Lexer::new(&mut diagnostics, &source);
            let parser = Parser::new(lexer);
            let module = parser.parse_module();
            let mut checker = Checker::new(&mut diagnostics);
            if let Ok(module) = module {
                checker.check_module(&module);
            }

            path.set_extension("ite.md");
            let mut contents = String::new();
            contents.push_str(&format!("# Checker Test: `{}`\n", stringify!($name)));
            if !diagnostics.is_empty() {
                contents.push_str("\n");
                contents.push_str("## Errors\n");
                contents.push_str(&diagnostics.markdown_list());
            }

            fs::write(path, contents).unwrap();
        }
    };
}

mod checker;
