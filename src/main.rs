#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]

mod data;
pub mod error;
mod intern;
mod location;
mod parse;
mod preprocess;
mod scope;

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
int main()
{
    char ch;
    _Bool isVowel = false;

    printf("Enter an alphabet: ");
    scanf("%c",&ch);

    if(ch=='a'||ch=='A'||ch=='e'||ch=='E'||ch=='i'||ch=='I'
            ||ch=='o'||ch=='O'||ch=='u'||ch=='U')
    {
        isVowel = true;

    }
    if (isVowel == true)
        printf("%c is a Vowel", ch);
    else
        printf("%c is a Consonant", ch);
    return 0;
}
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

    let mut out = Vec::new();
    loop {
        match parser.next() {
            Some(i) => out.push(i),
            None => break,
        }
    }
    println!("{:#?}", out);
    println!("{:?}", parser.warnings());
    for i in processor.pending_tokens {
        println!("{:?}", i);
    }
}
