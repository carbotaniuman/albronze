use super::*;

use crate::Source;
use codespan::Files;

// A little copy-pasting can't hurt right?
macro_rules! define_test {
    ($name: ident) => {
        #[test]
        fn $name() {
            let contents =
                arcstr::literal!(include_str!(concat!("files/", stringify!($name), ".c")));
            let mut files = Files::new();
            let id = files.add(
                concat!(stringify!($name), ".c"),
                Source {
                    code: contents.clone(),
                    path: concat!("files/", stringify!($name), ".c").into(),
                },
            );

            {
                let mut processor = Preprocessor::new(FileManager::new(), false);

                processor.preprocess_file(id, contents.clone());
                let $name = processor.pending_tokens;

                // Have a slightly nicer expression
                // in the snapshot files.
                insta::assert_display_snapshot!(pretty_print($name));
            }
        }
    };
    (with_comments $name: ident) => {
        #[test]
        fn $name() {
            let contents =
                arcstr::literal!(include_str!(concat!("files/", stringify!($name), ".c")));
            let mut files = Files::new();
            let id = files.add(
                concat!(stringify!($name), ".c"),
                Source {
                    code: contents.clone(),
                    path: concat!("files/", stringify!($name), ".c").into(),
                },
            );

            {
                let mut processor = Preprocessor::new(FileManager::new(), true);

                processor.preprocess_file(id, contents.clone());
                let $name = processor.pending_tokens;

                // Have a slightly nicer expression
                // in the snapshot files.
                insta::assert_display_snapshot!(pretty_print($name));
            }
        }
    };
}

// It is a weird day when you're copying code from
// the preprocessor iceberg to use for your tests.
// Behold the ~~power~~ horror of the C preprocessor.
// https://jadlevesque.github.io/PPMP-Iceberg

define_test! {adjacent_paste}
define_test! {with_comments basic}
define_test! {boline}
define_test! {with_comments comment_and_space}
define_test! {concatenation}
define_test! {counting}
define_test! {defect_268_1}
define_test! {defect_268_2}
define_test! {guide}
define_test! {list}
define_test! {nested_function}
define_test! {with_comments newline}
define_test! {no_arg}
define_test! {recursive_self_ref}
define_test! {recursive}
define_test! {scmd}
define_test! {sequence_iteration}
define_test! {stringify}
define_test! {with_comments vararg_stringify}
define_test! {vararg}
define_test! {weird_param_1}
define_test! {weird_param_2}
define_test! {with_comments whitespace}
