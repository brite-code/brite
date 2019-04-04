extern crate brite;

macro_rules! test {
    ($name:ident) => {
        #[test]
        fn $name() {
            use brite::checker::avt::simple_statement_conversion;
            use brite::compiler::js::compile_module;
            use brite::diagnostics::DiagnosticsCollection;
            use brite::parser::{ast, Lexer, Parser};
            use std::fs;
            use std::io::prelude::*;
            use std::path::PathBuf;

            let mut path = PathBuf::from(file!());
            path.set_file_name(stringify!($name));
            path.set_extension("ite");

            let source = fs::read_to_string(&path).unwrap();

            let mut diagnostics = DiagnosticsCollection::new();
            let lexer = Lexer::new(&mut diagnostics, &source);
            let parser = Parser::new(lexer);
            let module = parser.parse_module().unwrap();

            if module.declarations.len() != 1 {
                unimplemented!();
            }

            let declaration = &module.declarations[0];

            let block = match declaration {
                ast::Declaration::Function(function) => {
                    if function.name.identifier.as_str() != "main" {
                        unimplemented!();
                    }
                    if function.function.parameters.len() != 0 {
                        unimplemented!();
                    }
                    if function.function.return_type.is_some() {
                        unimplemented!();
                    }
                    &function.function.body
                }
                ast::Declaration::Class(_) => unimplemented!(),
            };

            let statements = compile_module(
                &block
                    .statements
                    .iter()
                    .map(simple_statement_conversion)
                    .collect(),
            );

            path.set_extension("ite.md");
            let mut file = fs::File::create(path).unwrap();
            write!(&mut file, "# Compiler Test: `{}`\n", stringify!($name)).unwrap();
            if !diagnostics.is_empty() {
                write!(&mut file, "\n## Errors\n{}", diagnostics.markdown_list()).unwrap();
            }

            write!(&mut file, "\n## JS\n```js\n").unwrap();
            for statement in statements {
                statement.write(&mut file, 0).unwrap();
                write!(&mut file, "\n").unwrap();
            }
            write!(&mut file, "```\n").unwrap();
        }
    };
}

mod compiler;
