extern crate brite;

macro_rules! test {
    ($name:ident) => {
        #[test]
        fn $name() {
            use brite::source::{parse, Document};
            use std::fs;
            use std::path::PathBuf;

            let mut path = PathBuf::from(file!());
            path.set_file_name(stringify!($name));
            path.set_extension("ite");
            let document = Document::read(path.clone()).unwrap();
            let (diagnostics, _) = parse(&document);
            path.set_extension("err");
            let mut contents = String::new();
            diagnostics.iter().for_each(|diagnostic| {
                contents.push_str(&diagnostic.to_simple_string(&document, path.parent()));
                contents.push('\n');
            });
            fs::write(path, contents).unwrap();
        }
    };
}

mod parser;
