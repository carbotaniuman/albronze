use super::*;
use super::expand::CppToken;

use crate::analyze::PureAnalyzer;
use crate::analyze::new_fold::Folder;
use crate::parse::Parser;

impl Preprocessor {
    pub(super) fn if_boolean_expr(&mut self, lexer: &mut Lexer) -> Result<bool, Locatable<CppError>> {
        let folder = crate::analyze::PreprocessorFolder;

        let iter = Self::iter_tokens_one_line(lexer).map(|x| x.unwrap());

        let a = self.preprocessor_expr(iter.into_iter())?;
        let mut error = ErrorHandler::default();
        let a = a.truthy(&mut error);
        
        let expr = folder.const_fold(&a).unwrap();

        use crate::data::LiteralValue;
        use crate::hir::ExprType;
        let ExprType::Literal(LiteralValue::Int(x)) = expr.expr else {
            todo!()
        };
        
        Ok(x == 1)
    }

    fn preprocessor_expr(
        &mut self,
        mut lexed: impl Iterator<Item = Locatable<TokenKind>>
    ) -> Result<crate::hir::Expr, Locatable<CppError>> {
        let mut cpp_tokens: Vec<CppToken> = Vec::with_capacity(lexed.size_hint().1.unwrap_or_default());

        while let Some(token) = lexed.next() {
            let token = match token {
                // #if defined(a)
                Locatable {
                    data: TokenKind::Identifier(name),
                    location,
                } if name == "defined".into() => {
                    let def = Self::defined(&mut lexed, location)?;
                    let literal = if self.definitions.contains_key(&def) {
                        "1".into()
                    } else {
                        "0".into()
                    };
                    location.with(TokenKind::Literal(LiteralKind::Number, literal))
                }
                _ => token,
            };
            cpp_tokens.push(CppToken::Token(token, false));
        }

        let mut hide_set = IndexSet::new();
        self.do_expand_macro(&mut Lexer::noop(), &mut cpp_tokens, &mut hide_set);

        let cpp_tokens = cpp_tokens.into_iter()
            .filter_map(|x| x.token())
            .map(|mut token| {
                if let TokenKind::Identifier(_) = token.data {
                    token.data = TokenKind::Literal(LiteralKind::Number, "0".into());
                }
                Ok(token)
            });

        let mut parser = Parser::new(cpp_tokens.into_iter(), false);
        let expr = parser.expr().unwrap();
        if !parser.is_empty() {
            panic!("should have only an expression {:?}", self.definitions.get(&"__GLIBC_USE".into()));
        }

        Ok(PureAnalyzer::new().expr(expr))
    }

    fn defined(
        mut lexed: impl Iterator<Item = Locatable<TokenKind>>,
        location: Location,
    ) -> Result<InternedStr, Locatable<CppError>> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum State {
            Start,
            SawParen,
            SawId(InternedStr),
        }
        
        let mut state = State::Start;
        loop {
            return match lexed.next() {
                None => Err(
                    location.with(CppError::EndOfFile("defined(identifier)").into())
                ),
                Some(Locatable {
                    data: TokenKind::Identifier(def),
                    location,
                }) => match state {
                    State::Start => Ok(def),
                    State::SawParen => {
                        state = State::SawId(def);
                        continue;
                    }
                    State::SawId(_) => Err(
                        location.with(CppError::UnexpectedToken("right paren", TokenKind::Identifier(def)))
                    ),
                },
                Some(Locatable {
                    data: TokenKind::LeftParen,
                    location,
                }) => match state {
                    State::Start => {
                        state = State::SawParen;
                        continue;
                    }
                    _ => Err(
                        location.with(
                            CppError::UnexpectedToken("identifier or right paren", TokenKind::LeftParen)
                        )
                    ),
                },
                Some(Locatable {
                    data: TokenKind::RightParen,
                    location,
                }) => match state {
                    State::Start => Err(
                        location.with(CppError::UnexpectedToken("identifier or left paren", TokenKind::RightParen))
                    ),
                    State::SawParen => Err(
                        location.with(CppError::UnexpectedToken("identifier", TokenKind::RightParen))
                    ),
                    State::SawId(def) => Ok(def),
                },
                Some(other) => Err(other.location.with(CppError::UnexpectedToken("identifier", other.data))),
            };
        }
    }
}