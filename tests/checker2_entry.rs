// extern crate brite;

// macro_rules! test {
//     ($name:ident) => {
//         #[test]
//         fn $name() {
//             use brite::checker::precheck_module;
//             use brite::diagnostics::DiagnosticsCollection;
//             use brite::parser::{Document, Lexer, Parser};
//             use std::fs;
//             use std::path::PathBuf;

//             let mut path = PathBuf::from(file!());
//             path.set_file_name(stringify!($name));
//             path.set_extension("ite");

//             let source = fs::read_to_string(&path).unwrap();

//             let mut diagnostics = DiagnosticsCollection::new();
//             let document = Document::new(source);
//             let lexer = Lexer::new(&mut diagnostics, &document);
//             let parser = Parser::new(lexer);
//             let module = parser.parse_module();
//             if let Ok(module) = module {
//                 precheck_module(&mut diagnostics, &module);
//             }

//             path.set_extension("ite.md");
//             let mut contents = String::new();
//             contents.push_str(&format!("# Checker Test: `{}`\n", stringify!($name)));
//             if !diagnostics.is_empty() {
//                 contents.push_str("\n");
//                 contents.push_str("## Errors\n");
//                 contents.push_str(&diagnostics.markdown_list(&document));
//             }

//             fs::write(path, contents).unwrap();
//         }
//     };
// }

// mod checker2;
