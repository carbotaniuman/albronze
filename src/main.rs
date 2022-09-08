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
int main(){
    int num, rem, reverse_num, temp, start, end;

    printf("Enter the lower limit: ");
    scanf("%d",&start);

    printf("Enter the upper limit: ");
    scanf("%d",&end);

    printf("Palindrome numbers between %d and %d are: ",start,end);
    for(num=start;num<=end;num++){
        temp=num;
        reverse_num=0;
        while(temp){
            rem=temp%10;
            temp=temp/10;
            reverse_num=reverse_num*10+rem;
        }
        if(num==reverse_num)
            printf("%d ",num);
    }
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
    // for i in processor.pending_tokens {
    //     println!("{:?}", i);
    // }
}
