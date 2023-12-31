use std::convert::TryFrom;

use super::*;
use crate::parse::ast::*;

use crate::preprocess::DigraphKind;

#[derive(Debug)]
enum InternalDeclaratorType {
    Id(InternedStr),
    Pointer {
        qualifiers: Vec<DeclarationSpecifier>,
    },
    Array {
        size: Option<Box<Expr>>,
    },
    Function {
        params: Vec<TypeName>,
        varargs: bool,
    },
}

#[derive(Debug)]
struct InternalDeclarator {
    current: InternalDeclaratorType,
    next: Option<Box<InternalDeclarator>>,
}

impl<I: Iterator<Item = Lexeme>> Parser<I> {
    /// ```yacc
    /// external_declaration
    /// : function_definition
    /// | declaration
    /// ;
    ///
    /// declaration
    /// : declaration_specifiers ';'
    /// | declaration_specifiers init_declarator_list ';'
    /// ;
    /// ```
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#external_declaration>
    pub fn external_declaration(&mut self) -> SyntaxResult<Locatable<ExternalDeclaration>> {
        let (specifiers, specifier_locations) = self.specifiers()?;

        // allow `int;`
        if let Some(token) = self.match_next(&TokenKind::Semicolon) {
            let location = token.location.maybe_merge_opt(specifier_locations);
            self.error_handler.warn(Warning::EmptyDeclaration, location);
            let empty_decl = ExternalDeclaration::Declaration(Declaration {
                specifiers,
                declarators: Vec::new(),
            });
            return Ok(Locatable::new(empty_decl, location));
        }

        let declarator = self.init_declarator()?;
        let mut location = declarator.location.maybe_merge_opt(specifier_locations);
        if self.peek_token() == Some(&TokenKind::LeftBrace(DigraphKind::Standard)) {
            // int i = 1 {}
            let func = match declarator.data.declarator.decl {
                DeclaratorType::Function(func) => func,
                _ => return Err(location.with(SyntaxError::NotAFunction(declarator.data))),
            };
            // int f() = 1 { }
            if let Some(init) = declarator.data.init {
                return Err(location.with(SyntaxError::FunctionInitializer(init)));
            }

            let body = self.compound_statement()?;
            let location = location.maybe_merge(body.location);
            // int () {}
            let err = location.with(SyntaxError::MissingFunctionName);
            let id = declarator.data.declarator.id.ok_or(err)?;
            let def = FunctionDefinition {
                id,
                body: body.data,
                specifiers,
                declarator: func,
            };
            return Ok(Locatable::new(ExternalDeclaration::Function(def), location));
        }
        let mut decls = vec![declarator];
        let has_typedef = specifiers
            .iter()
            .any(|s| *s == DeclarationSpecifier::Unit(crate::parse::ast::UnitSpecifier::Typedef));
        while self.match_next(&TokenKind::Semicolon).is_none() {
            self.expect(TokenKind::Comma)?;
            let decl = self.init_declarator()?;
            location = location.maybe_merge(decl.location);
            decls.push(decl);
        }
        if has_typedef {
            // `int *;` is caught later
            for id in decls.iter().filter_map(|d| d.data.declarator.id) {
                self.typedefs.insert(id, ());
            }
        }
        let declaration = Declaration {
            specifiers,
            declarators: decls,
        };
        Ok(Locatable::new(
            ExternalDeclaration::Declaration(declaration),
            location,
        ))
    }
    pub fn type_name(&mut self) -> SyntaxResult<Locatable<TypeName>> {
        let (specifiers, specifier_locations) = self.specifiers()?;
        let maybe_declarator = self.declarator(true)?;
        let (location, declarator) = match maybe_declarator {
            None => (
                specifier_locations,
                Declarator {
                    decl: DeclaratorType::End,
                    id: None,
                },
            ),
            Some(decl) => (
                Some(decl.location.maybe_merge_opt(specifier_locations)),
                decl.data.parse_declarator(),
            ),
        };
        let location = match location {
            None => {
                assert_eq!(declarator.decl, DeclaratorType::End);
                return Err(self.next_location().with(SyntaxError::ExpectedType));
            }
            Some(l) => l,
        };
        let type_name = TypeName {
            specifiers,
            declarator,
        };
        Ok(Locatable::new(type_name, location))
    }
    fn specifiers(&mut self) -> SyntaxResult<(Vec<DeclarationSpecifier>, Option<Location>)> {
        let mut specifiers = Vec::new();
        let mut all_locs = None;
        let mut seen_typedef = false;
        while let Some(&TokenKind::Keyword(keyword)) = self.peek_token() {
            let location = self.next_token().unwrap().location;
            let spec = match keyword {
                Keyword::Struct => self.struct_specifier(true, location)?,
                Keyword::Union => self.struct_specifier(false, location)?,
                Keyword::Enum => self.enum_specifier(location)?,
                Keyword::UserTypedef(name) => {
                    // absolute hack: allow awful code like `typedef int I; { I I; }`
                    if !seen_typedef {
                        seen_typedef = true;
                        Locatable::new(DeclarationSpecifier::Typedef(name), location)
                    } else {
                        self.unput(Some(Locatable::new(TokenKind::Identifier(name), location)));
                        break;
                    }
                }
                other if !other.is_decl_specifier() => {
                    let err = SyntaxError::ExpectedDeclSpecifier(keyword);
                    return Err(location.with(err));
                }
                _ => Locatable::new(keyword.try_into().unwrap(), location),
            };
            all_locs = all_locs.map_or(Some(spec.location), |existing: Location| {
                Some(existing.maybe_merge(spec.location))
            });
            specifiers.push(spec.data);
        }
        Ok((specifiers, all_locs))
    }
    /// ```yacc
    /// struct_or_union_specifier
    /// : (struct | union) '{' struct_declaration + '}'
    /// | (struct | union) identifier '{' struct_declaration + '}'
    /// | (struct | union) identifier
    /// ;
    /// ```
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#struct_or_union_specifier>
    fn struct_specifier(
        &mut self,
        is_struct: bool,
        mut start: Location,
    ) -> SyntaxResult<Locatable<DeclarationSpecifier>> {
        let name = self.match_id().map(|id| {
            start = start.maybe_merge(id.location);
            id.data
        });
        let members = if let Some(token) =
            self.match_next(&TokenKind::LeftBrace(DigraphKind::Standard))
        {
            start = start.maybe_merge(token.location);
            let mut members = Vec::new();
            loop {
                if let Some(token) = self.match_next(&TokenKind::RightBrace(DigraphKind::Standard))
                {
                    start = start.maybe_merge(token.location);
                    break;
                }
                if let Some(token) = self.match_next(&TokenKind::Semicolon) {
                    self.error_handler.warn(
                        Warning::ExtraneousSemicolon("struct declaration is not allowed by ISO"),
                        token.location,
                    );
                    continue;
                }
                let decl = self.struct_declaration_list()?;
                start = start.maybe_merge(decl.location);
                members.push(decl.data);
            }
            Some(members)
        } else {
            None
        };
        let spec = StructSpecifier { name, members };
        let spec = if is_struct {
            DeclarationSpecifier::Struct(spec)
        } else {
            DeclarationSpecifier::Union(spec)
        };
        Ok(Locatable::new(spec, start))
    }

    /// ```yacc
    /// struct_declaration: (type_specifier | type_qualifier)+ struct_declarator_list ';'
    ///
    /// struct_declarator_list: struct_declarator (',' struct_declarator)* ;
    ///
    /// struct_declarator
    /// : declarator
    /// | ':' constant_expr  // bitfield, not supported
    /// | declarator ':' constant_expr
    /// ;
    /// ```
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#struct_declaration>
    fn struct_declaration_list(&mut self) -> SyntaxResult<Locatable<ast::StructDeclarationList>> {
        //use data::lex::LocationTrait;
        let (specifiers, mut spec_location) = self.specifiers()?;
        let mut declarators = Vec::new();
        let location = loop {
            if let Some(token) = self.match_next(&TokenKind::Semicolon) {
                break token.location.maybe_merge_opt(spec_location);
            }
            let decl = if self.peek_token() != Some(&TokenKind::Colon) {
                self.declarator(true)?.map(|d| {
                    spec_location = Some(d.location.maybe_merge_opt(spec_location));
                    let mut decl = d.data.parse_declarator();
                    if decl.id.is_none() {
                        self.error_handler.error(
                            SyntaxError::Generic("struct members must have an id".into()),
                            d.location,
                        );
                        decl.id = Some("<unnamed member>".into());
                    }
                    decl
                })
            } else {
                None
            };
            let bitfield = if let Some(token) = self.match_next(&TokenKind::Colon) {
                let size = self.ternary_expr()?;
                spec_location = Some(token.location.maybe_merge(size.location));
                Some(size)
            } else {
                None
            };
            declarators.push(ast::StructDeclarator { decl, bitfield });
            if self.match_next(&TokenKind::Comma).is_none() {
                break self.expect(TokenKind::Semicolon)?.location;
            }
        };
        let decl_list = ast::StructDeclarationList {
            specifiers,
            declarators,
        };
        Ok(Locatable::new(
            decl_list,
            location.maybe_merge_opt(spec_location),
        ))
    }
    /// ```yacc
    /// enum_specifier
    /// : 'enum' '{' enumerator_list '}'
    /// | 'enum' identifier '{' enumerator_list '}'

    // this is not valid for declaring an enum, but it's fine for an enum we've already seen
    // e.g. `enum E { A }; enum E e;`

    /// | 'enum' identifier
    /// ;
    ///
    /// enumerator_list
    /// : enumerator
    /// | enumerator_list ',' enumerator
    /// ;
    ///
    /// enumerator
    /// : IDENTIFIER
    /// | IDENTIFIER '=' constant_expression
    /// ;
    /// ```
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#enum_specifier>

    // we've already seen an `enum` token,, `location` is where we saw it
    fn enum_specifier(
        &mut self,
        mut location: Location,
    ) -> SyntaxResult<Locatable<DeclarationSpecifier>> {
        let name = self.match_id().map(|id| {
            location = location.maybe_merge(id.location);
            id.data
        });
        let body = if let Some(token) =
            self.match_next(&TokenKind::LeftBrace(DigraphKind::Standard))
        {
            location = location.maybe_merge(token.location);
            let mut body = Vec::new();
            loop {
                if let Some(token) = self.match_next(&TokenKind::RightBrace(DigraphKind::Standard))
                {
                    location = location.maybe_merge(token.location);
                    break;
                }
                let enumerator = self.expect_id()?;
                let value = if self.match_next(&TokenKind::Equal).is_some() {
                    Some(self.ternary_expr()?)
                } else {
                    None
                };
                body.push((enumerator.data, value));
                if self.match_next(&TokenKind::Comma).is_none() {
                    let token = self.expect(TokenKind::RightBrace(DigraphKind::Standard))?;
                    location = location.maybe_merge(token.location);
                    break;
                }
            }
            Some(body)
        } else {
            None
        };
        let decl = DeclarationSpecifier::Enum {
            name,
            members: body,
        };
        Ok(Locatable::new(decl, location))
    }

    fn init_declarator(&mut self) -> SyntaxResult<Locatable<ast::InitDeclarator>> {
        let decl = self.declarator(false)?;
        let init = if self.match_next(&TokenKind::Equal).is_some() {
            Some(self.initializer()?)
        } else {
            None
        };
        // TODO: this location is wrong
        let location = self.last_location;
        let decl = decl.ok_or_else(|| location.with(SyntaxError::ExpectedDeclarator))?;
        Ok(decl.map(|d| ast::InitDeclarator {
            declarator: InternalDeclarator::parse_declarator(d),
            init,
        }))
    }

    fn merge_decls(
        current: Locatable<InternalDeclaratorType>,
        next: Option<Locatable<InternalDeclarator>>,
    ) -> Locatable<InternalDeclarator> {
        if let Some(next) = next {
            let location = current.location.maybe_merge(next.location);
            let decl = InternalDeclarator {
                current: current.data,
                next: Some(Box::new(next.data)),
            };
            Locatable::new(decl, location)
        } else {
            current.map(|data| InternalDeclarator {
                current: data,
                next: None,
            })
        }
    }
    fn declarator(
        &mut self,
        allow_abstract: bool,
    ) -> SyntaxResult<Option<Locatable<InternalDeclarator>>> {
        let mut pointer_decls = Vec::new();
        // NOTE: outdated comment
        // decls coming earlier in the Vec have lower precedence than the ones coming later
        // e.g. `*const *volatile p` would look like `vec![Pointer(const), Pointer(volatile), Id("p")]`
        // and  `*const (*f)()` would look like `vec![Pointer(const), Function, Pointer, Id("f")]`
        // anything to the left of a `Function` represents the return type
        // anything to the right represents a declarator with higher precedence
        // the `Id` should always be the last declarator in the Vec
        while let Some(Locatable { mut location, .. }) = self.match_next(&TokenKind::Star) {
            let mut qualifiers = Vec::new();
            // *const volatile p
            while let Some(Locatable {
                location: keyword_loc,
                data: TokenKind::Keyword(keyword),
            }) = self.match_any(&[
                &TokenKind::Keyword(Keyword::Const),
                &TokenKind::Keyword(Keyword::Volatile),
                &TokenKind::Keyword(Keyword::Restrict),
                &TokenKind::Keyword(Keyword::Atomic),
                &TokenKind::Keyword(Keyword::ThreadLocal),
            ]) {
                location = location.maybe_merge(keyword_loc);
                qualifiers.push(keyword.try_into().unwrap());
            }
            let current = Locatable::new(InternalDeclaratorType::Pointer { qualifiers }, location);
            pointer_decls.push(current);
        }
        let mut decl = self.direct_declarator(allow_abstract)?;
        while let Some(pointer) = pointer_decls.pop() {
            decl = Some(Self::merge_decls(pointer, decl));
        }
        Ok(decl)
    }
    /*
     * Originally written as follows:
     * direct_declarator
     *  : identifier
     *  | '(' declarator ')'
     *  | direct_declarator '[' ']'
     *  | direct_declarator '[' constant_expr ']'
     *  | direct_declarator '(' ')'
     *  | direct_declarator '(' parameter_type_list ')'
     *  ;
     * <http://www.quut.com/c/ANSI-C-grammar-y.html#direct_declarator>
     *
     * Additionally, we combine abstract_declarators, because most of the code is the same.
     * direct_abstract_declarator
     *  : '(' abstract_declarator ')'
     *  | '[' ']'
     *  | '[' constant_expr ']'
     *  | direct_abstract_declarator '[' ']'
     *  | direct_abstract_declarator '[' constant_expr ']'
     *  | '(' ')'
     *  | '(' parameter_type_list ')'
     *  | direct_abstract_declarator '(' ')'
     *  | direct_abstract_declarator '(' parameter_type_list ')'
     *  ;
     * <http://www.quut.com/c/ANSI-C-grammar-y.html#direct_abstract_declarator>
     *
     * Because we can't handle left-recursion, we rewrite it as follows:
     * direct_abstract_declarator
     *   : '(' abstract_declarator ')' postfix_type*
     *   | identifier postfix_type*
     *   | postfix_type*  /* only for abstract_declarators */
     *   ;
     *
     * postfix_type:
     *   : '[' ']'
     *   | '[' constant_expr ']'
     *   | '(' ')'
     *   | '(' parameter_type_list ')'
     *   ;
     * ```
     *
     *   How do we tell abstract_declarator and parameter_type_list apart?
     *   parameter_type_list starts with declaration specifiers, abstract_declarator doesn't:
     *   https://stackoverflow.com/questions/56410673/how-should-int-fint-be-parsed
     */
    fn direct_declarator(
        &mut self,
        allow_abstract: bool,
    ) -> SyntaxResult<Option<Locatable<InternalDeclarator>>> {
        // let _guard = self.recursion_check();
        // we'll pass this to postfix_type in just a second
        // if None, we didn't find an ID
        // should only happen if allow_abstract is true
        let decl: Option<Locatable<InternalDeclarator>> = match self.peek_token() {
            Some(TokenKind::Identifier(_)) => {
                Some(self.next_token().unwrap().map(|data| match data {
                    TokenKind::Identifier(id) => InternalDeclarator {
                        current: InternalDeclaratorType::Id(id),
                        next: None,
                    },
                    _ => panic!("peek() should always return the same thing as next()"),
                }))
            }
            // handled by postfix_type
            Some(TokenKind::LeftBracket(_)) if allow_abstract => None,
            Some(TokenKind::LeftParen) => {
                // this is the reason we need to save next - otherwise we
                // consume LeftParen without postfix_type ever seeing it
                match self.peek_next_token() {
                    // HACK: catch function declarators with implicit int
                    // If we see code like the following: `int f(());`,
                    // this does _not_ mean a parenthesized declarator. Instead,
                    // this is a function declarator with an implicit `int` (compare `int f(int ())`).
                    // This will later be desugared by the analyzer to `int f(int (*)())`.
                    // TODO: this does _not_ catch more complex types like `int f((()))`
                    // (which clang parses as `int f(int (int ()))`) - it instead treats them as `int f(())`.
                    // However, this is so cursed I don't expect it to be a real problem in the real world
                    Some(TokenKind::RightParen) => {
                        let left_paren = self.expect(TokenKind::LeftParen).unwrap().location;
                        let right_paren = self.expect(TokenKind::RightParen).unwrap().location;
                        let decl = InternalDeclarator {
                            current: InternalDeclaratorType::Function {
                                params: Vec::new(),
                                varargs: false,
                            },
                            next: None,
                        };
                        Some(Locatable::new(decl, left_paren.maybe_merge(right_paren)))
                    }
                    // parameter_type_list, leave it for postfix_type
                    // need to check allow_abstract because we haven't seen an ID at
                    // this point
                    Some(TokenKind::Keyword(k)) if k.is_decl_specifier() && allow_abstract => None,
                    // abstract_declarator - could be an error,
                    // but if so we'll catch it later
                    _ => {
                        // the one we already matched
                        self.expect(TokenKind::LeftParen)
                            .expect("peek_next_token should be accurate");
                        let declarator = self.declarator(allow_abstract)?;
                        self.expect(TokenKind::RightParen)?;
                        declarator
                    }
                }
            }
            _ if allow_abstract => None,
            Some(x) => {
                let err = Locatable::new(
                    SyntaxError::Generic(format!("expected variable name or '(', got '{}'", x)),
                    self.next_location(),
                );
                self.panic();
                return Err(err);
            }
            None => {
                return Err(self.next_location().with(SyntaxError::Generic(
                    "expected variable name or '(', got <end-of-of-file>".into(),
                )));
            }
        };
        self.postfix_type(decl, allow_abstract)
    }
    /*
     * not in original reference, see comments to `direct_declarator`
     *
     * rewritten grammar:
     *   postfix_type:
     *        '[' ']'
     *      | '[' constant_expr ']'
     *      | '(' ')'
     *      | '(' parameter_type_list ')'
     *      | /* empty */
     *      ;
     */
    #[inline]
    fn postfix_type(
        &mut self,
        mut prefix: Option<Locatable<InternalDeclarator>>,
        allow_abstract: bool,
    ) -> SyntaxResult<Option<Locatable<InternalDeclarator>>> {
        while let Some(data) = self.peek_token() {
            let current = match data {
                // Array; Specified in section 6.7.6.2 of the C11 spec
                TokenKind::LeftBracket(_) => {
                    self.expect(TokenKind::LeftBracket(DigraphKind::Standard))
                        .unwrap();
                    if let Some(token) = self.match_next(&TokenKind::Keyword(Keyword::Static)) {
                        if !allow_abstract {
                            self.error_handler.push_error(Locatable::new(
                                SyntaxError::StaticInConcreteArray,
                                token.location,
                            ));
                        }
                    }
                    let (size, location) = if let Some(token) =
                        self.match_next(&TokenKind::RightBracket(DigraphKind::Standard))
                    {
                        (None, token.location)
                    } else {
                        let expr = Box::new(self.expr()?);
                        (
                            Some(expr),
                            self.expect(TokenKind::RightBracket(DigraphKind::Standard))?
                                .location,
                        )
                    };
                    Locatable::new(InternalDeclaratorType::Array { size }, location)
                }
                TokenKind::LeftParen => self.parameter_type_list()?,
                _ => break,
            };
            prefix = Some(Self::merge_decls(current, prefix))
        }
        Ok(prefix)
    }
    /*
     * function parameters
     * reference grammar:
     *
     *  parameter_type_list:
     *        parameter_list
     *      | parameter_list ',' ELLIPSIS
     *      ;
     *
     *  parameter_list:
     *        parameter_declaration
     *      | parameter_list ',' parameter_declaration
     *      ;
     *
     *  parameter_declaration:
     *        declaration_specifiers declarator
     *      | declaration_specifiers
     *      | declaration_specifiers abstract_declarator
     *      ;
     *
     * <http://www.quut.com/c/ANSI-C-grammar-y.html#parameter_type_list>
     */
    fn parameter_type_list(&mut self) -> SyntaxResult<Locatable<InternalDeclaratorType>> {
        let left_paren = self
            .expect(TokenKind::LeftParen)
            .expect("parameter_type_list should only be called with '(' as the next token")
            .location;
        let mut params = vec![];
        if let Some(right_paren) = self.match_next(&TokenKind::RightParen) {
            return Ok(Locatable::new(
                InternalDeclaratorType::Function {
                    params,
                    varargs: false,
                },
                left_paren.maybe_merge(right_paren.location),
            ));
        }
        loop {
            if self.match_next(&TokenKind::Ellipsis).is_some() {
                let right_paren = self.expect(TokenKind::RightParen)?.location;
                return Ok(Locatable::new(
                    InternalDeclaratorType::Function {
                        params,
                        varargs: true,
                    },
                    left_paren.maybe_merge(right_paren),
                ));
            }
            let param = self.type_name()?;
            params.push(param.data);
            if self.match_next(&TokenKind::Comma).is_none() {
                let right_paren = self.expect(TokenKind::RightParen)?.location;
                let location = left_paren.maybe_merge(right_paren);
                return Ok(Locatable::new(
                    InternalDeclaratorType::Function {
                        params,
                        varargs: false,
                    },
                    location,
                ));
            }
        }
    }
    fn initializer(&mut self) -> SyntaxResult<Initializer> {
        // initializer_list
        if self
            .match_next(&TokenKind::LeftBrace(DigraphKind::Standard))
            .is_some()
        {
            self.aggregate_initializer()
        } else {
            let expr = self.assignment_expr()?;
            Ok(Initializer::Scalar(Box::new(expr)))
        }
    }

    // handle char[][3] = {{1,2,3}}, but also = {1,2,3} and {{1}, 2, 3}
    // NOTE: this does NOT consume {} except for sub-elements
    fn aggregate_initializer(&mut self) -> SyntaxResult<Initializer> {
        // let _guard = self.recursion_check();
        let mut elems = vec![];
        while self
            .match_next(&TokenKind::RightBrace(DigraphKind::Standard))
            .is_none()
        {
            let next = if self
                .match_next(&TokenKind::LeftBrace(DigraphKind::Standard))
                .is_some()
            {
                self.aggregate_initializer()?
            } else {
                // scalar
                self.initializer()?
            };
            elems.push(next);
            // NOTE: this allows trailing commas
            if self.match_next(&TokenKind::Comma).is_none() {
                self.expect(TokenKind::RightBrace(DigraphKind::Standard))?;
                break;
            };
        }
        Ok(Initializer::Aggregate(elems))
    }
}

impl InternalDeclarator {
    fn parse_declarator(self) -> Declarator {
        use InternalDeclaratorType::*;

        let mut id = None;
        let mut current = DeclaratorType::End;
        let mut declarator = Some(self);
        while let Some(decl) = declarator {
            current = match decl.current {
                Id(i) => {
                    id = Some(i);
                    current
                }
                Pointer { qualifiers } => DeclaratorType::Pointer {
                    to: Box::new(current),
                    qualifiers,
                },
                Array { size } => DeclaratorType::Array {
                    of: Box::new(current),
                    size,
                },
                Function { params, varargs } => DeclaratorType::from(ast::FunctionDeclarator {
                    return_type: Box::new(current),
                    params,
                    varargs,
                }),
            };
            declarator = decl.next.map(|x| *x);
        }
        Declarator { decl: current, id }
    }
}

impl TryFrom<Keyword> for DeclarationSpecifier {
    type Error = ();
    #[rustfmt::skip]
    fn try_from(k: Keyword) -> Result<DeclarationSpecifier, ()> {
        use ast::UnitSpecifier;

        // TODO: get rid of this macro and store a `enum Keyword { Qualifier(Qualifier), etc. }` instead
        macro_rules! change_enum {
            ($val: expr, $source: path, $dest: ident, $($name: ident),* $(,)?) => {
                match $val {
                    $(<$source>::$name => Ok(DeclarationSpecifier::Unit(UnitSpecifier::$name)),)*
                    _ => Err(()),
                }
            }
        }

        change_enum!(k, Keyword, DeclarationSpecifier,
            Const, Volatile, Restrict, Atomic, ThreadLocal,
            Unsigned, Signed,
            Bool, Char, Short, Int, Long, Float, Double, Void,
            Complex, Imaginary, VaList,
            Extern, Static, Auto, Register, Typedef,
            Inline, NoReturn,
        )
    }
}

impl TokenKind {
    pub(super) fn is_decl_specifier(&self) -> bool {
        match self {
            TokenKind::Keyword(k) => k.is_decl_specifier(),
            //TokenKind::Id(id) => typedefs.get(id).is_some(),
            _ => false,
        }
    }
}

impl Keyword {
    pub(super) fn is_decl_specifier(self) -> bool {
        use Keyword::*;
        match self {
            // type specifier
            Unsigned | Signed | Bool | Char | Short | Int | Long | Float | Double | Void
            // complex type specifier
            | Struct | Union | Enum | VaList | Complex | Imaginary
            // user-defined type
            | UserTypedef(_)
            // storage class
            | Extern | Static | Auto | Register | Typedef
            // qualifier
            | Const | Volatile | Restrict | Atomic | ThreadLocal
            // function qualifier
            | Inline | NoReturn => true,
            _ => false,
        }
    }
}
