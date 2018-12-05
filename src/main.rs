extern crate brite;
extern crate unicode_xid;

use brite::source::{parse, Document};
use std::env;
use std::path::PathBuf;

fn main() {
    let mut args = env::args();
    args.next();
    let path = args.next().expect("Expecting a file path.");
    let path = PathBuf::from(&path);
    let document = Document::read(path).unwrap();
    let (diagnostics, module) = parse(&document);
    println!("{:#?}", diagnostics);
    println!("{:#?}", module);
}
