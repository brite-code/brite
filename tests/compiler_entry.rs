extern crate brite;

macro_rules! test {
    ($name:ident) => {
        #[test]
        fn $name() {
            use brite::checker::Checker;
            use brite::compiler::js::Compiler;
            use brite::diagnostics::DiagnosticsCollection;
            use brite::parser::{Lexer, Parser};
            use std::fs;
            use std::io::prelude::*;
            use std::path::PathBuf;

            let mut path = PathBuf::from(file!());
            path.set_file_name(stringify!($name));
            path.set_extension("ite");

            let source = fs::read_to_string(&path).unwrap();

            let mut diagnostics = DiagnosticsCollection::new();
            let lexer = Lexer::new(&mut diagnostics, &source);
            let module = Parser::new(lexer).parse_module().unwrap();
            let module = Checker::new(&mut diagnostics).check_module(&module);
            let program = Compiler::new().compile_module(&module);

            path.set_extension("ite.md");
            let mut file = fs::File::create(path).unwrap();
            write!(&mut file, "# Compiler Test: `{}`\n", stringify!($name)).unwrap();
            if !diagnostics.is_empty() {
                write!(&mut file, "\n## Errors\n{}", diagnostics.markdown_list()).unwrap();
            }

            write!(&mut file, "\n## JS\n```js\n").unwrap();
            program.write(&mut file).unwrap();
            write!(&mut file, "```\n").unwrap();
        }
    };
}

mod compiler;
