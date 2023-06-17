use crate::analyze::{Folder, FoldError};
use crate::analyze::PureAnalyzer;
use crate::location::Locatable;
use crate::parse::Parser;
use crate::preprocess::{FileManager, Keyword, Preprocessor, TokenKind};
use crate::{get_str, Files, Source};
use arcstr::ArcStr;
use std::str::FromStr;

fn process(folder: impl Folder, contents: ArcStr) -> Result<crate::hir::ExprType, Locatable<FoldError>> {
    let mut files = Files::new();
    let id = files.add(
        "<generated>",
        Source {
            code: contents.clone(),
            path: concat!("<generated>").into(),
        },
    );

    let preprocessed = {
        let mut processor = Preprocessor::new(FileManager::new(), false);

        processor.preprocess_file(id, contents.clone());
        processor.pending_tokens
    };

    let parsed = {
        let mut parser = Parser::new(
            preprocessed
                .into_iter()
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

        let expr = parser.ternary_expr().expect("test should have only a conditional expression");
        if !parser.is_empty() {
            panic!("test should have only an expression");
        }
        expr
    };

    let analyzed = PureAnalyzer::new().expr(parsed);

    folder.const_fold(&analyzed).map(|a| a.expr)
}

macro_rules! define_test {
    ($name: ident, $folder : expr, $contents: literal) => {
        #[test]
        fn $name() {
            insta::assert_debug_snapshot!(process($folder, arcstr::literal!($contents)));
        }
    };
}

// Preprocessor Things
use crate::analyze::PreprocessorFolder;
define_test!(simple_literal, PreprocessorFolder, "5");
define_test!(addition, PreprocessorFolder, "5 + 5");
define_test!(subtraction, PreprocessorFolder, "5 - 5");
define_test!(multiplication, PreprocessorFolder, "5 * 5");
define_test!(division, PreprocessorFolder, "5 / 5");
define_test!(bitwise_not, PreprocessorFolder, "~5");

// Our AST construction is just busted, we'll fix this later
define_test!(logical_not_1, PreprocessorFolder, "!5");
define_test!(logical_not_2, PreprocessorFolder, "!0");
define_test!(ternary_1, PreprocessorFolder, "1 ? 2 : 3");
define_test!(ternary_2, PreprocessorFolder, "!1 ? 2 : 3");
