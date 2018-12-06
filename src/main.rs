extern crate brite;
extern crate unicode_xid;

use brite::source::{parse, Document};
use std::env;
use std::io;
use std::path::PathBuf;

fn main() -> Result<(), io::Error> {
    let mut args = env::args();
    args.next();
    let path = args.next().expect("Expecting a file path.");
    let path = PathBuf::from(&path);
    let document = Document::read(path)?;
    let (diagnostics, _module) = parse(&document);
    let path = document.path();
    let path = path.strip_prefix(env::current_dir()?).unwrap_or(path);
    for diagnostic in diagnostics.iter() {
        println!(
            "{}({},{}): {}",
            path.to_string_lossy(),
            diagnostic.range().start().line(&document) + 1,
            diagnostic.range().start().character(&document) + 1,
            diagnostic.message().to_simple_string()
        );
    }
    Ok(())
}
