#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]

pub mod data;
pub mod error;
mod intern;
mod location;
mod parse;
mod preprocess;
mod scope;

use crate::preprocess::pretty_print;

use intern::*;

use arcstr::ArcStr;
use std::path::PathBuf;

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
        r#"#define str(x) #x
#define f(x) x
#define glue(x, y) x ## y
#define EMPTY
/* These are based on PR 4492, we mustn't lose padding tokens when
    scanning ahead for a '(' and failing to find it.  */
#define A(x) B x
#define B(x)
#define C A
#define D() A

glue (EMPTY 4, 4) EMPTY;
A(Q) C(Q) D()Q D():
f
bar
A
bad
f (g) str
(
1
2
) f
(bam) baz
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
    // for i in processor.pending_tokens {
    //     println!("{:?}", i);
    // }
}
