use super::*;

impl Preprocessor {
    // Consumes tokens like this:
    // before:
    // #define f(a, b, c) a + b + c
    //        ^
    // after:
    // eats entire line
    pub(super) fn define(&mut self, lexer: &mut Lexer) -> Result<(), Locatable<CppError>> {
        let macro_id = match self.expect_id(lexer, false) {
            Ok(ident) => Ok(ident),
            Err(Locatable { data, location }) if data.is_unexpected_newline() => {
                Err(location.with(CppError::EmptyDefine))
            }
            Err(err) => Err(err),
        }?;

        let mut params = None;

        // Eat the params for the macro. If there is an EOF/NL then
        // we treat it as an empty object macro. If there is a '(' directly
        // next we parse it as a function-like macro. `start` will have the first
        // token of the replacement list, which may be a newline token.
        let start_offset = lexer.offset();
        let mut start = lexer.next();
        match start {
            Some(Ok(Locatable { data, location })) => {
                use TokenKind::*;

                match data {
                    Whitespace(kind) => {
                        use WhitespaceKind::*;
                        match kind {
                            Newline => {
                                return self.define_macro(
                                    macro_id,
                                    Macro {
                                        builtin: false,
                                        kind: MacroKind::Object {
                                            replacement: Vec::new(),
                                        },
                                    },
                                )
                            }
                            _ => start = lexer.next_non_whitespace(),
                        }
                    }

                    LeftParen => {
                        params = Some(self.expect_macro_params(lexer, start_offset)?);
                        start = lexer.next_non_whitespace();
                    }

                    _ => self
                        .error_handler
                        .get_mut()
                        .warn(Warning::NoSpaceAfterMacroIdentifier, macro_id.location),
                }
            }
            Some(Err(e)) => return Err(e.map(|x| x.into())),
            None => {
                return self.define_macro(
                    macro_id,
                    Macro {
                        builtin: false,
                        kind: MacroKind::Object {
                            replacement: Vec::new(),
                        },
                    },
                )
            }
        }

        // Now it's time to eat the replacement list of the macro
        let mac = Macro {
            builtin: false,
            kind: if let Some((params, variadic)) = params {
                MacroKind::Function {
                    replacement: self.expect_fn_macro_replacement_list(lexer, &params, start)?,
                    params,
                    variadic,
                }
            } else {
                MacroKind::Object {
                    replacement: self.expect_obj_macro_replacement_list(lexer, start)?,
                }
            },
        };

        return self.define_macro(macro_id, mac);
    }

    fn define_macro(
        &mut self,
        Locatable {
            data: name,
            location,
        }: Locatable<InternedStr>,
        definition: Macro,
    ) -> Result<(), Locatable<CppError>> {
        use std::collections::hash_map::Entry;
        match self.definitions.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert((definition, location));
                Ok(())
            }
            Entry::Occupied(entry) => {
                // This behavior is defined by the spec in section 6.10.3p1
                if &entry.get().0 != &definition {
                    Err(location.with(CppError::IncompatibleRedefinition(name)))
                } else {
                    Ok(())
                }
            }
        }
    }

    // Consumes tokens like this:
    // before:
    // #define f(a, b, c) a + b + c
    //           ^
    // after:
    // #define f(a, b, c) a + b + c
    //                   ^
    fn expect_macro_params(
        &mut self,
        lexer: &mut Lexer,
        start: u32,
    ) -> Result<(IndexSet<InternedStr>, VariadicType), Locatable<CppError>> {
        let mut arguments = IndexSet::new();
        let mut vararg = VariadicType::None;
        loop {
            match lexer.next_non_whitespace() {
                None => {
                    return Err(Locatable::new(
                        CppError::EndOfFile("identifier or ')'"),
                        lexer.span(start),
                    ));
                }
                Some(Err(err)) => return Err(err.map(|x| x.into())),
                Some(Ok(Locatable {
                    data: TokenKind::RightParen,
                    ..
                })) => {
                    return Ok((arguments, vararg));
                }
                Some(Ok(Locatable {
                    data: TokenKind::Ellipsis,
                    ..
                })) => {
                    vararg = VariadicType::Standard;
                }
                Some(Ok(Locatable {
                    data: TokenKind::Identifier(id),
                    location,
                })) => {
                    if !arguments.insert(id) {
                        return Err(location.with(CppError::DuplicateParameter(id.to_string())));
                    }
                }
                Some(Ok(Locatable {
                    data: other,
                    location,
                })) => {
                    return Err(Locatable::new(
                        CppError::UnexpectedToken("identifier or ')'", other),
                        location,
                    ));
                }
            }
            match lexer.next_non_whitespace() {
                None => {
                    return Err(Locatable::new(
                        CppError::EndOfFile("identifier or ')'"),
                        lexer.span(start),
                    ))
                }
                Some(Err(err)) => return Err(err.map(|x| x.into())),
                Some(Ok(other)) => {
                    use TokenKind::*;

                    match other.data {
                        Comma if vararg == VariadicType::None => {
                            continue;
                        }
                        RightParen => return Ok((arguments, vararg)),
                        t => {
                            return Err(Locatable::new(
                                if vararg == VariadicType::None {
                                    CppError::UnexpectedToken("identifier or ')'", t)
                                } else {
                                    CppError::UnexpectedToken("')' after '...'", t)
                                },
                                lexer.span(start),
                            ))
                        }
                    }
                }
            }
        }
    }

    // Consumes tokens like this:
    // before:
    // #define F a + b + c
    //           ^
    // after:
    // eats entire line
    fn expect_obj_macro_replacement_list(
        &mut self,
        lexer: &mut Lexer,
        first: Option<Result<Locatable<TokenKind>, Locatable<LexError>>>,
    ) -> Result<Vec<Locatable<TokenKind>>, Locatable<CppError>> {
        let mut tokens: Vec<Locatable<TokenKind>> = Vec::new();

        let mut cur = first;
        loop {
            match cur {
                Some(Ok(cur @ Locatable { data, location })) => {
                    use TokenKind::*;
                    use WhitespaceKind::*;

                    match data {
                        HashHash(_) => {
                            let hashhash_location = location;

                            let next = match lexer.next_non_whitespace() {
                                Some(Ok(next @ Locatable { data, .. })) => match data {
                                    HashHash(_) | Whitespace(Newline) => {
                                        return Err(hashhash_location.with(
                                            CppError::HashHashMissingParameter { start: false },
                                        ))
                                    }
                                    _ => next,
                                },
                                Some(Err(err)) => return Err(err.map(|x| x.into())),
                                None => {
                                    return Err(hashhash_location
                                        .with(CppError::HashHashMissingParameter { start: false }))
                                }
                            };

                            tokens.push(cur);
                            tokens.push(next);
                        }
                        Whitespace(Newline) => {
                            if let Some(Locatable {
                                location,
                                data: Whitespace(_),
                            }) = tokens.last_mut()
                            {
                                tokens.pop();
                            }
                            break;
                        }
                        Whitespace(..) => {
                            if let Some(Locatable {
                                location,
                                data: Whitespace(_),
                            }) = tokens.last_mut()
                            {
                                if location.is_directly_before(cur.location) {
                                    location.merge_span(cur.location.span);
                                }
                            } else if tokens.len() != 0 {
                                tokens.push(
                                    cur.location.with(Whitespace(WhitespaceKind::MacroSpace)),
                                );
                            }
                        }
                        _ => {
                            tokens.push(cur);
                        }
                    }
                }
                Some(Err(err)) => return Err(err.map(|x| x.into())),
                None => {
                    break;
                }
            }

            cur = lexer.next();
        }

        Ok(tokens)
    }

    // Consumes tokens like this:
    // before:
    // #define f(a, b, c) a + b + c
    //                    ^
    // after:
    // eats entire line
    fn expect_fn_macro_replacement_list(
        &mut self,
        lexer: &mut Lexer,
        params: &IndexSet<InternedStr>,
        first: Option<Result<Locatable<TokenKind>, Locatable<LexError>>>,
    ) -> Result<Vec<ReplacementKind>, Locatable<CppError>> {
        let mut tokens: Vec<ReplacementKind> = Vec::new();

        let mut cur = first;
        loop {
            match cur {
                Some(Ok(cur @ Locatable { data, location })) => {
                    use TokenKind::*;
                    use WhitespaceKind::*;

                    match data {
                        HashHash(_) => {
                            let hashhash_location = location;

                            let next = match lexer.next_non_whitespace() {
                                Some(Ok(next @ Locatable { data, location })) => match data {
                                    HashHash(_) | Whitespace(Newline) => {
                                        return Err(hashhash_location.with(
                                            CppError::HashHashMissingParameter { start: false },
                                        ))
                                    }
                                    Hash(_) => {
                                        let hash_location = location;

                                        use TokenKind::*;
                                        match lexer.next_non_whitespace() {
                                            Some(Ok(Locatable { data, location })) => match data {
                                                Identifier(id) => {
                                                    if let Some(index) = params.get_index_of(&id) {
                                                        BaseReplacement::Stringify(
                                                            location.with(index),
                                                        )
                                                    } else if get_str!(id) == "__VA_ARGS__" {
                                                        BaseReplacement::StringifyVarargs(location)
                                                            .into()
                                                    } else {
                                                        return Err(hash_location
                                                            .with(CppError::HashMissingParameter));
                                                    }
                                                }
                                                _ => {
                                                    return Err(hash_location
                                                        .with(CppError::HashMissingParameter))
                                                }
                                            },
                                            Some(Err(err)) => return Err(err.map(|x| x.into())),
                                            None => {
                                                return Err(hash_location
                                                    .with(CppError::HashMissingParameter))
                                            }
                                        }
                                    }
                                    _ => BaseReplacement::Token(next),
                                },
                                Some(Err(err)) => return Err(err.map(|x| x.into())),
                                None => {
                                    return Err(hashhash_location
                                        .with(CppError::HashHashMissingParameter { start: false }))
                                }
                            };

                            if let Some(t) = tokens.pop() {
                                let to_push = match t {
                                    ReplacementKind::Base(b) => {
                                        ReplacementKind::Concatenate(vec![b, next])
                                    }
                                    ReplacementKind::Concatenate(mut v) => {
                                        v.push(next);
                                        ReplacementKind::Concatenate(v)
                                    }
                                };

                                tokens.push(to_push);
                            } else {
                                return Err(hashhash_location
                                    .with(CppError::HashHashMissingParameter { start: true }));
                            }
                        }
                        Hash(_) => {
                            let hash_location = location;

                            use TokenKind::*;
                            match lexer.next_non_whitespace() {
                                Some(Ok(Locatable { data, location })) => match data {
                                    Identifier(id) => {
                                        if let Some(index) = params.get_index_of(&id) {
                                            tokens.push(
                                                BaseReplacement::Stringify(location.with(index))
                                                    .into(),
                                            )
                                        } else if get_str!(id) == "__VA_ARGS__" {
                                            tokens.push(
                                                BaseReplacement::StringifyVarargs(location).into(),
                                            );
                                        } else {
                                            return Err(
                                                hash_location.with(CppError::HashMissingParameter)
                                            );
                                        }
                                    }
                                    _ => {
                                        return Err(
                                            hash_location.with(CppError::HashMissingParameter)
                                        )
                                    }
                                },
                                Some(Err(err)) => return Err(err.map(|x| x.into())),
                                None => {
                                    return Err(hash_location.with(CppError::HashMissingParameter))
                                }
                            }
                        }
                        Whitespace(Newline) => {
                            if let Some(ReplacementKind::Base(BaseReplacement::Token(
                                Locatable {
                                    location,
                                    data: Whitespace(_),
                                },
                            ))) = tokens.last_mut()
                            {
                                tokens.pop();
                            }
                            break;
                        }
                        Whitespace(..) => {
                            if let Some(ReplacementKind::Base(BaseReplacement::Token(
                                Locatable {
                                    location,
                                    data: Whitespace(_),
                                },
                            ))) = tokens.last_mut()
                            {
                                if location.is_directly_before(cur.location) {
                                    location.merge_span(cur.location.span);
                                }
                            } else if tokens.len() != 0 {
                                tokens.push(
                                    BaseReplacement::Token(
                                        cur.location.with(Whitespace(WhitespaceKind::MacroSpace)),
                                    )
                                    .into(),
                                );
                            }
                        }
                        _ => {
                            tokens.push(BaseReplacement::Token(cur).into());
                        }
                    }
                }
                Some(Err(err)) => return Err(err.map(|x| x.into())),
                None => {
                    break;
                }
            }

            cur = lexer.next();
        }

        Ok(tokens)
    }
}
