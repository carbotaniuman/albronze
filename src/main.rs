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
use crate::preprocess::{pretty_print, Keyword, TokenKind};

use intern::*;

use arcstr::ArcStr;
use std::path::PathBuf;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct Source {
    pub code: ArcStr,
    pub path: PathBuf,
}

impl AsRef<str> for Source {
    fn as_ref(&self) -> &str {
        self.code.as_ref()
    }
}

pub type Files = codespan::Files<Source>;

fn main() {
    let mut manager = preprocess::FileManager::new();
    let mut processor = preprocess::Preprocessor::new(manager, true);

    let value = arcstr::format!(
        "{}",
        r#"
#define A(x, y) x##y
#define B(x, y) A(x, y)

#define FOO 3
#define BAR 6

A(FOO, BAR)
B(FOO, BAR)        
"#
    );
    let mut files = Files::new();
    let id = files.add(
        "laser",
        Source {
            code: value.clone(),
            path: "".into(),
        },
    );

    processor.preprocess_file(id, value);
    println!(
        "{}",
        format!("{}", pretty_print(processor.pending_tokens.clone()))
    );

    for i in &processor.pending_tokens {
        println!("{:?}", i);
    }

    let x = processor.pending_tokens.clone();
    println!("{:?}", processor.error_handler);

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
        println!();
    }
    println!("{:?}", analyzer.warnings());
}
