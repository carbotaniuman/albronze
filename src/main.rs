#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]

mod analyze;
mod arch;
mod data;
pub mod error;
mod hir;
mod intern;
mod location;
mod parse;
mod preprocess;
mod scope;

use crate::analyze::Analyzer;
use crate::parse::Parser;
use crate::preprocess::{ Keyword, TokenKind };
use crate::location::SourceKind;

use intern::*;

use std::str::FromStr;

fn main() {
    let mut manager = preprocess::FileManager::new();
    let mut processor = preprocess::Preprocessor::new(manager, true);

    let value = arcstr::format!("{}", std::fs::read_to_string("test.c").unwrap());

    processor.preprocess_file(SourceKind::Generated, value);

    let x = processor.pending_tokens.clone();
    eprintln!("Errors: {:?}", processor.error_handler);

    let mut parser = Parser::new(
        x.into_iter()
            .map(|t| {
                if let TokenKind::Identifier(id) = t.data {
                    if let Ok(keyword) = Keyword::from_str(get_str!(id)) {
                        return t.location.with(TokenKind::Keyword(keyword));
                    }
                }

                t
            })
            .map(|t| Ok(t)),
        false,
    );

    use crate::analyze::PureAnalyzer;

    let mut analyzer = Analyzer::new(parser, false);

    let mut out = Vec::new();
    loop {
        match analyzer.next() {
            Some(i) => out.push(i),
            None => break,
        }
    }
    
    

    for i in out {
        println!("{}", i.unwrap().data);
    }
    eprintln!("{:?}", analyzer.warnings());
}
