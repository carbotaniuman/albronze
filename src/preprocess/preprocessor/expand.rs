use super::*;

use std::iter::once;

#[derive(Copy, Clone, Debug, PartialEq)]
enum CppToken {
    Token(Locatable<TokenKind>, /* painted */ bool),
    EndMacro(InternedStr),
}

impl CppToken {
    fn token(self) -> Option<Locatable<TokenKind>> {
        match self {
            Self::Token(t, _) => Some(t),
            Self::EndMacro(_) => None,
        }
    }
}

impl Preprocessor {
    pub(super) fn process_next_token(&mut self, lexer: &mut Lexer, token: Locatable<TokenKind>) {
        let mut expansion_buffer: Vec<CppToken> = vec![CppToken::Token(token, false)];
        let mut hide_set = IndexSet::new();

        // println!("{:?}", token);
        // dbg!(&self.definitions);

        self.do_expand_macro(lexer, &mut expansion_buffer, &mut hide_set);

        self.pending_tokens
            .extend(expansion_buffer.into_iter().filter_map(|x| x.token()));
    }

    // The main algorithm used in this code is derived from
    // the description by Bradley Smith here:
    // https://marc.info/?l=boost&m=118835769257658
    //
    // Another reference used is Dave Prosser's
    // algorithm, documented here:
    // https://www.spinellis.gr/blog/20060626/
    fn do_expand_macro(
        &self,
        lexer: &mut Lexer,
        expansion_buffer: &mut Vec<CppToken>,
        disabling_contexts: &mut IndexSet<InternedStr>,
    ) {
        let mut cur_index = 0;

        while cur_index < expansion_buffer.len() {
            // println!("start: {:?}", &expansion_buffer[cur_index..]);
            // println!("contexts: {:?}", disabling_contexts);
            let token = &mut expansion_buffer[cur_index];
            // println!("cur token: {:?}", token);

            match *token {
                CppToken::Token(token, ref mut painted) if !*painted => {
                    if let TokenKind::Identifier(id) = token.data {
                        if let Some(m) = self.definitions.get(&id) {
                            // If we see this token in a disabling context, paint it
                            // to avoid future expansion, and then move to the next token.
                            // This painting occurs even if `m` is a function-like macro,
                            // and there are no parens (so the macro would not get called).
                            if disabling_contexts.get(&id).is_some() {
                                *painted = true;
                                cur_index += 1;

                                continue;
                            }

                            let call_location = token.location;

                            match &m.0.kind {
                                MacroKind::Object { replacement } => {
                                    disabling_contexts.insert(id);

                                    expansion_buffer.splice(
                                        cur_index..cur_index + 1,
                                        replacement
                                            .iter()
                                            .copied()
                                            .map(|x| CppToken::Token(x, false))
                                            .chain(once(CppToken::EndMacro(id))),
                                    );
                                }

                                MacroKind::Function {
                                    replacement,
                                    params,
                                    variadic,
                                } => {
                                    // This index represents the latest index that we
                                    // have processed, either in the main loop above,
                                    // or in the checks down below.
                                    let mut index = cur_index;

                                    let is_func_call = loop {
                                        // Bump the index to the next token. There may be
                                        // a better way of doing this, but I cannot think of
                                        // it at this time.
                                        index += 1;
                                        let token = self.buffer_or_next(
                                            lexer,
                                            expansion_buffer,
                                            disabling_contexts,
                                            &mut index,
                                        );

                                        let token = match token {
                                            Some(token) => token,
                                            None => {
                                                break false;
                                            }
                                        };

                                        match token.data {
                                            TokenKind::Whitespace(..) => continue,
                                            TokenKind::LeftParen => {
                                                break true;
                                            }
                                            _ => {
                                                break false;
                                            }
                                        }
                                    };

                                    if is_func_call {
                                        let args = self.expect_macro_args(
                                            lexer,
                                            expansion_buffer,
                                            disabling_contexts,
                                            params.len(),
                                            &mut index,
                                        );

                                        // Perform macro argument pre-scanning
                                        let mut expanded_args: Vec<Vec<CppToken>> =
                                            Vec::with_capacity(args.len());

                                        for arg in &args {
                                            let mut temp_buffer: Vec<CppToken> = arg.to_vec();

                                            self.do_expand_macro(
                                                &mut Lexer::noop(),
                                                &mut temp_buffer,
                                                disabling_contexts,
                                            );
                                            expanded_args.push(temp_buffer);
                                        }

                                        let replacement = self.replacement_for_function(
                                            call_location,
                                            replacement,
                                            params,
                                            *variadic,
                                            expansion_buffer,
                                            &args,
                                            &expanded_args,
                                        );

                                        disabling_contexts.insert(id);
                                        expansion_buffer.splice(
                                            cur_index..index + 1,
                                            replacement.chain(once(CppToken::EndMacro(id))),
                                        );

                                        // println!("after func: {:?}", expansion_buffer);
                                    } else {
                                        // This function-like macro isn't actually a call,
                                        // we simply move onwards.
                                        cur_index = index;
                                        continue;
                                    }
                                }
                            }

                            // Don't bump the current index, we
                            // rescan at the beginning of the replacement list.
                            continue;
                        }
                    }
                }
                CppToken::EndMacro(id) => {
                    disabling_contexts.remove(&id);
                }
                _ => {}
            }

            cur_index += 1;
        }
    }

    fn replacement_for_function(
        &self,
        call_location: Location,
        replacement: &[ReplacementKind],
        params: &IndexSet<InternedStr>,
        variadic: VariadicType,

        expansion_buffer: &[CppToken],
        args: &[Vec<CppToken>],
        expanded_args: &[Vec<CppToken>],
    ) -> impl Iterator<Item = CppToken> {
        let mut ret: Vec<Option<CppToken>> = Vec::new();

        if !(args.len() == 1 && params.len() == 0) {
            if args.len() > params.len() && variadic == VariadicType::None {
                todo!("extra macro args");
            }
        }

        if args.len() < params.len() {
            todo!("missing macro args {} {}", args.len(), params.len());
        }

        for t in replacement.iter() {
            let handle_base = |base: &BaseReplacement,
                               ret: &mut Vec<Option<CppToken>>,
                               concat: bool| {
                use BaseReplacement::*;
                match *base {
                    Token(t) => {
                        if let TokenKind::Identifier(id) = t.data {
                            if get_str!(id) == "__VA_ARGS__" {
                                if variadic == VariadicType::Standard {
                                    let slice = if !concat {
                                        &expanded_args[params.len()..]
                                    } else {
                                        &args[params.len()..]
                                    };

                                    if !slice.is_empty() {
                                        for i in slice {
                                            ret.extend(i.iter().copied().map(|x| Some(x)));

                                            ret.push(
                                                CppToken::Token(
                                                    Location::generated((0..1u32).into())
                                                        .with(TokenKind::Comma),
                                                    false,
                                                )
                                                .into(),
                                            );
                                        }
                                    } else {
                                        ret.push(None);
                                    }

                                    // Pop the last comma off
                                    ret.pop();
                                    return;
                                } else {
                                    // TODO: warn
                                    todo!()
                                }
                            } else if let Some(index) = params.get_index_of(&id) {
                                let new = if concat { args } else { expanded_args }[index]
                                    .iter()
                                    .copied()
                                    .map(|x| Some(x));
                                ret.extend(new);
                                return;
                            }
                        }

                        ret.push(CppToken::Token(t, false).into());
                    }
                    Stringify(Locatable { data, location }) => {
                        let tokens = args[data].iter().filter_map(|x| x.token()).map(|x| x.data);

                        // TODO: should this be generated location?
                        ret.push(
                            CppToken::Token(
                                location.with(TokenKind::Literal(
                                    LiteralKind::String(EncodingKind::Normal),
                                    Self::stringify(tokens),
                                )),
                                false,
                            )
                            .into(),
                        )
                    }
                    StringifyVarargs(location) => ret.push(
                        CppToken::Token(
                            location.with(TokenKind::Literal(
                                LiteralKind::String(EncodingKind::Normal),
                                Self::stringify(
                                    args[params.len()..]
                                        .iter()
                                        .flatten()
                                        .filter_map(|x| x.token())
                                        .map(|x| x.data),
                                ),
                            )),
                            false,
                        )
                        .into(),
                    ),
                }
            };

            use ReplacementKind::*;
            match t {
                Base(base) => {
                    handle_base(base, &mut ret, false);
                }
                Concatenate(replacements) => {
                    // The `Option` here represents placemarkers, but we can
                    // treat this as an implementation of an array linked list.
                    // This means that removing an element simply means setting
                    // it to `None`, saving us the pain of shifting a bunch
                    // of tokens left.
                    let mut first_arg: Vec<Option<CppToken>> = Vec::new();
                    let mut second_arg: Vec<Option<CppToken>> = Vec::new();

                    handle_base(&replacements[0], &mut first_arg, true);
                    if first_arg.is_empty() {
                        first_arg.push(None);
                    }

                    for i in replacements.iter().skip(1) {
                        handle_base(i, &mut second_arg, true);
                        if second_arg.is_empty() {
                            second_arg.push(None);
                        }

                        let mut first_tok: (Option<TokenKind>, usize) = (None, 0);
                        for (index, elements) in first_arg.iter().enumerate().rev() {
                            match elements {
                                Some(CppToken::Token(token, _)) if !token.data.is_whitespace() => {
                                    first_tok = (Some(token.data), index);
                                    break;
                                }
                                None => {
                                    first_tok.1 = index;
                                    break;
                                }
                                _ => {}
                            }
                        }

                        let mut second_tok: (Option<TokenKind>, usize) = (None, 0);
                        for (index, elements) in second_arg.iter().enumerate() {
                            match elements {
                                Some(CppToken::Token(token, _)) if !token.data.is_whitespace() => {
                                    second_tok = (Some(token.data), index);
                                    break;
                                }
                                None => {
                                    second_tok.1 = index;
                                    break;
                                }
                                _ => {}
                            }
                        }

                        match (first_tok.0, second_tok.0) {
                            (None, None) => {}
                            (None, Some(_)) => {}
                            (Some(_), None) => std::mem::swap(
                                &mut first_arg[first_tok.1],
                                &mut second_arg[second_tok.1],
                            ),
                            (Some(x), Some(y)) => {
                                assert!(!x.is_whitespace());
                                assert!(!y.is_whitespace());

                                let s = arcstr::ArcStr::from(format!("{x}{y}"));
                                let lexer =
                                    &mut Lexer::new(SourceKind::Generated, s.clone(), false, false);

                                if let Some(Ok(locatable)) = lexer.next() {
                                    if lexer.next().is_none() {
                                        // Really we want a deletion here, but swapping
                                        // in `None` doesn't change the semantics and
                                        // saves us from shifting a lot of elements.
                                        std::mem::swap(&mut first_arg[first_tok.1], &mut None);
                                        std::mem::swap(
                                            &mut second_arg[second_tok.1],
                                            &mut Some(CppToken::Token(locatable, false)),
                                        );
                                    } else {
                                        self.error_handler.borrow_mut().error(
                                            CppError::HashHashInvalid(s.clone()),
                                            call_location,
                                        )
                                    }
                                } else {
                                    self.error_handler
                                        .borrow_mut()
                                        .error(CppError::HashHashInvalid(s.clone()), call_location)
                                }
                            }
                        }

                        first_arg.extend(second_arg.drain(..));
                    }

                    ret.extend(first_arg.into_iter())
                }
            }
        }

        ret.into_iter().filter_map(|x| x)
    }

    fn stringify(tokens: impl Iterator<Item = TokenKind>) -> InternedStr {
        struct EscapeStringCStyle<'a>(&'a str);

        impl std::fmt::Display for EscapeStringCStyle<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                use std::fmt::Write;

                for c in self.0.chars() {
                    match c {
                        '\"' => write!(f, r#"\""#)?,
                        '\\' => write!(f, r#"\\"#)?,
                        c => f.write_char(c)?,
                    }
                }

                Ok(())
            }
        }

        let mut temp = String::new();

        for token in tokens {
            use std::fmt::Write;
            use LiteralKind::*;
            use TokenKind::*;

            // We need to handle strings and chars a little bit differently,
            // as we need to put escaped quotes around them, whiel also escaping
            // inner quotes and backslahes.
            match token {
                Literal(String(encoding), s) => write!(
                    &mut temp,
                    r#"{}\"{}\""#,
                    encoding,
                    EscapeStringCStyle(get_str!(s))
                ),
                Literal(Char(encoding), s) => write!(
                    &mut temp,
                    r#"{}\'{}\'"#,
                    encoding,
                    EscapeStringCStyle(get_str!(s))
                ),
                _ => write!(&mut temp, "{}", token),
            }
            .unwrap()
        }

        InternedStr::get_or_intern(temp.trim())
    }

    // Consumes tokens like this:
    // before:
    // #define f(a, (b, c), d)
    //           ^
    // after:
    // #define f(a, (b, c), d)
    //                        ^
    fn expect_macro_args(
        &self,
        lexer: &mut Lexer,
        expansion_buffer: &mut Vec<CppToken>,
        disabling_contexts: &mut IndexSet<InternedStr>,
        expected_params: usize,
        index: &mut usize,
    ) -> Vec<Vec<CppToken>> {
        let mut nesting = 0_usize;

        let mut ret = Vec::new();
        let mut temp: Vec<CppToken> = Vec::new();

        loop {
            *index += 1;
            let token = self.buffer_or_next(lexer, expansion_buffer, disabling_contexts, index);

            let token = match token {
                Some(token) => token,
                None => todo!("unterminated macro args"),
            };

            use TokenKind::*;
            match token.data {
                Comma if nesting == 0 && ret.len() < expected_params => {
                    if matches!(
                        temp.last().and_then(|x| x.token()).map(|x| x.data),
                        Some(Whitespace(_))
                    ) {
                        temp.pop();
                    }

                    ret.push(std::mem::take(&mut temp));
                }
                RightParen if nesting == 0 => {
                    if matches!(
                        temp.last().and_then(|x| x.token()).map(|x| x.data),
                        Some(Whitespace(_))
                    ) {
                        temp.pop();
                    }

                    ret.push(std::mem::take(&mut temp));
                    return ret;
                }
                Whitespace(..) => {
                    if let Some(CppToken::Token(
                        Locatable {
                            location,
                            data: Whitespace(_),
                        },
                        _,
                    )) = temp.last_mut()
                    {
                        if location.is_directly_before(token.location) {
                            location.merge_span(token.location.span);
                        }
                    } else if temp.len() != 0 {
                        temp.push(CppToken::Token(
                            token.location.with(Whitespace(WhitespaceKind::MacroSpace)),
                            false,
                        ));
                    }
                }
                c => {
                    if c == LeftParen {
                        nesting += 1
                    } else if c == RightParen {
                        nesting -= 1;
                    }

                    temp.push(expansion_buffer[*index])
                }
            }
        }
    }

    fn buffer_or_next(
        &self,
        lexer: &mut Lexer,
        expansion_buffer: &mut Vec<CppToken>,
        disabling_contexts: &mut IndexSet<InternedStr>,
        index: &mut usize,
    ) -> Option<Locatable<TokenKind>> {
        loop {
            if *index < expansion_buffer.len() {
                let token = expansion_buffer[*index];
                match token {
                    CppToken::Token(token, ..) => return Some(token),
                    CppToken::EndMacro(id) => {
                        disabling_contexts.remove(&id);
                    }
                }

                *index += 1;
            } else {
                let next = lexer.next();
                let token = match next {
                    Some(token) => token,
                    None => {
                        // TODO
                        // let location = lexer.span(lexer.offset());
                        // lexer.err_loc(LexError::NoNewlineAtEOF, location);

                        return None;
                    }
                };

                let token = match token {
                    Ok(token) => token,
                    Err(e) => todo!("{:?}", e),
                };

                if let TokenKind::Identifier(ident) = token.data {
                    if self.poisoned.contains(&ident) {
                        todo!("poisoned")
                    }
                }
                expansion_buffer.push(CppToken::Token(token, false));
            }
        }
    }
}
