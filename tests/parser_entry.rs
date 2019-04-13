extern crate brite;

macro_rules! test {
    ($name:ident) => {
        #[test]
        fn $name() {
            use brite::diagnostics::DiagnosticsCollection;
            use brite::parser::{Document, Lexer, Parser};
            use std::fs;
            use std::path::PathBuf;
            use typed_arena::Arena;

            let mut path = PathBuf::from(file!());
            path.set_file_name(stringify!($name));
            path.set_extension("ite");

            let source = fs::read_to_string(&path).unwrap();

            let mut diagnostics = DiagnosticsCollection::new();
            let arena = Arena::new();
            let document = Document::new(source);
            let lexer = Lexer::new(&mut diagnostics, &document);
            let parser = Parser::new(&arena, lexer);
            let module = parser.parse_module();

            path.set_extension("ite.md");
            let mut contents = String::new();
            contents.push_str(&format!("# Parser Test: `{}`\n", stringify!($name)));
            if !diagnostics.is_empty() {
                contents.push_str("\n");
                contents.push_str("## Errors\n");
                contents.push_str(&diagnostics.markdown_list(&document));
            }
            if let Ok(module) = &module {
                contents.push_str("\n");
                contents.push_str("## AST\n");
                contents.push_str("```\n");
                for declaration in &module.declarations {
                    contents.push_str(&declaration.print_lisp(&document, 80));
                    contents.push_str("\n");
                }
                contents.push_str("```\n");
            }

            fs::write(path, contents).unwrap();

            assert!(module.is_ok() || module.is_err() && !diagnostics.is_empty());
        }
    };
}

mod parser;
