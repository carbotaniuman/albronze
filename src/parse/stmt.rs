use super::*;
use crate::parse::ast::*;

use crate::preprocess::DigraphKind;

type StmtResult = SyntaxResult<Stmt>;

impl<I: Iterator<Item = Lexeme>> Parser<I> {
    pub fn compound_statement(&mut self) -> SyntaxResult<Locatable<CompoundStatement>> {
        let mut location = self
            .expect(TokenKind::LeftBrace(DigraphKind::Standard))
            .expect("compound_statement should be called with '{' as the next token")
            .location;
        let mut stmts = vec![];
        let mut pending_errs = vec![];
        while self.peek_token() != Some(&TokenKind::RightBrace(DigraphKind::Standard)) {
            match self.statement() {
                Ok(stmt) => {
                    location = location.maybe_merge(stmt.location);
                    stmts.push(stmt);
                }
                Err(err) => {
                    self.panic();
                    pending_errs.push(err);
                    // prevent infinite loops if there's a syntax error at EOF
                    if self.peek_token().is_none() {
                        break;
                    }
                }
            }
        }
        if self
            .expect(TokenKind::RightBrace(DigraphKind::Standard))
            .is_err()
        {
            assert!(self.peek_token().is_none()); // from the 'break' above
            let actual_err = self.last_location.with(SyntaxError::Generic(
                "unclosed '{' delimeter at end of file".into(),
            ));
            pending_errs.push(actual_err);
        }
        if let Some(err) = pending_errs.pop() {
            self.error_handler.extend_errors(pending_errs.into_iter());
            Err(err)
        } else {
            Ok(Locatable::new(stmts, location))
        }
    }
    fn declaration(&mut self) -> SyntaxResult<Stmt> {
        let decl = self.external_declaration()?;
        match decl.data.into_declaration() {
            Err(err) => Err(decl.location.with(err)),
            Ok(declaration) => Ok(Stmt::new(StmtType::Decl(declaration), decl.location)),
        }
    }
    /// ```yacc
    /// statement
    /// : labeled_statement
    /// | compound_statement
    /// | expression_statement
    /// | selection_statement
    /// | iteration_statement
    /// | jump_statement
    /// ;
    ///
    /// labeled_statement
    /// : identifier ':' statement
    /// | CASE constant_expr ':' statement
    /// | DEFAULT ':' statement
    /// ;
    /// ```
    pub fn statement(&mut self) -> SyntaxResult<Stmt> {
        // let _guard = self.recursion_check();
        // take out 2 guards since this goes through `compound_statement` before calling itself again
        // let _guard2 = self.recursion_check();
        match self.peek_token() {
            Some(TokenKind::LeftBrace(_)) => Ok(self.compound_statement()?.map(StmtType::Compound)),
            Some(TokenKind::Keyword(k)) => match k {
                // labeled_statement (excluding labels)
                Keyword::Case => {
                    let kw = self.next_token().unwrap();
                    let expr = self.expr()?;
                    self.expect(TokenKind::Colon)?;
                    let inner = Box::new(self.statement()?);
                    Ok(Stmt {
                        location: kw.location.maybe_merge(expr.location),
                        data: StmtType::Case(Box::new(expr), inner),
                    })
                }
                Keyword::Default => {
                    let kw = self.next_token().unwrap();
                    self.expect(TokenKind::Colon)?;
                    let inner = self.statement()?;
                    Ok(Stmt {
                        data: StmtType::Default(Box::new(inner)),
                        location: kw.location,
                    })
                }

                // selection_statement
                Keyword::If => self.if_statement(),
                Keyword::Switch => self.switch_statement(),

                // iteration_statement
                Keyword::While => self.while_statement(),
                Keyword::Do => self.do_while_statement(),
                Keyword::For => self.for_statement(),

                // jump_statement
                Keyword::Goto => self.goto_statement(),
                Keyword::Continue => {
                    let kw = self.next_token().unwrap();
                    self.expect(TokenKind::Semicolon)?;
                    Ok(Stmt {
                        data: StmtType::Continue,
                        location: kw.location,
                    })
                }
                Keyword::Break => {
                    let kw = self.next_token().unwrap();
                    self.expect(TokenKind::Semicolon)?;
                    Ok(Stmt {
                        data: StmtType::Break,
                        location: kw.location,
                    })
                }
                Keyword::Return => self.return_statement(),

                // start of an expression statement
                Keyword::Sizeof
                | Keyword::StaticAssert
                | Keyword::Alignas
                | Keyword::Alignof
                | Keyword::Generic => self.expression_statement(),
                decl if decl.is_decl_specifier() => self.declaration(),
                other => {
                    let err = SyntaxError::NotAStatement(*other);
                    Err(self.next_location().with(err))
                }
            },
            Some(TokenKind::Semicolon) => {
                let Locatable { location, .. } = self.next_token().expect("peek is broken");
                Ok(Stmt {
                    data: Default::default(),
                    location,
                })
            }
            Some(TokenKind::Identifier(_)) => {
                let locatable = self.next_token().unwrap();
                let id = match locatable.data {
                    TokenKind::Identifier(id) => Locatable {
                        data: id,
                        location: locatable.location,
                    },
                    _ => unreachable!("peek should always be the same as next"),
                };
                if self.match_next(&TokenKind::Colon).is_some() {
                    return Ok(Stmt {
                        data: StmtType::Label(id.data, Box::new(self.statement()?)),
                        location: id.location,
                    });
                }
                let is_typedef = self.typedefs.get(&id.data).is_some();
                self.unput(Some(Locatable {
                    data: TokenKind::Identifier(id.data),
                    location: id.location,
                }));
                if is_typedef {
                    self.declaration()
                } else {
                    self.expression_statement()
                }
            }
            _ => self.expression_statement(),
        }
    }
    // expr ;
    fn expression_statement(&mut self) -> SyntaxResult<Stmt> {
        let expr = self.expr()?;
        let end = self.expect(TokenKind::Semicolon)?;
        Ok(Stmt {
            location: expr.location.maybe_merge(end.location),
            data: StmtType::Expr(expr),
        })
    }
    // return (expr)? ;
    fn return_statement(&mut self) -> StmtResult {
        let ret_token = self.expect(TokenKind::Keyword(Keyword::Return)).unwrap();
        let expr = self.expr_opt(TokenKind::Semicolon)?;
        Ok(Stmt {
            location: ret_token
                .location
                .maybe_merge_opt(expr.as_ref().map(|l| l.location)),
            data: StmtType::Return(expr),
        })
    }
    /// if_statement:
    ///     IF '(' expr ')' statement
    ///   | IF '(' expr ')' statement ELSE statement
    fn if_statement(&mut self) -> StmtResult {
        let start = self
            .expect(TokenKind::Keyword(Keyword::If))
            .expect("parser shouldn't call if_statement without an if");
        self.expect(TokenKind::LeftParen)?;
        let condition = self.expr()?;
        self.expect(TokenKind::RightParen)?;
        let body = self.statement()?;
        let otherwise = if self
            .match_next(&TokenKind::Keyword(Keyword::Else))
            .is_some()
        {
            // NOTE: `if (1) ; else ;` is legal!
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        let stmt = StmtType::If(condition, Box::new(body), otherwise);
        Ok(Stmt {
            data: stmt,
            location: start.location,
        })
    }
    /// switch_statement: SWITCH '(' expr ')' statement
    fn switch_statement(&mut self) -> StmtResult {
        let start = self.expect(TokenKind::Keyword(Keyword::Switch))?;
        self.expect(TokenKind::LeftParen)?;
        let expr = self.expr()?;
        self.expect(TokenKind::RightParen)?;
        let body = self.statement()?;
        let stmt = StmtType::Switch(expr, Box::new(body));
        Ok(Stmt {
            data: stmt,
            location: start.location,
        })
    }
    /// while_statement: WHILE '(' expr ')' statement
    fn while_statement(&mut self) -> StmtResult {
        let start = self.expect(TokenKind::Keyword(Keyword::While))?;
        self.expect(TokenKind::LeftParen)?;
        let condition = self.expr()?;
        self.expect(TokenKind::RightParen)?;
        let body = self.statement()?;
        Ok(Stmt {
            data: StmtType::While(condition, Box::new(body)),
            location: start.location,
        })
    }
    /// do_while_statement: DO statement WHILE '(' expr ')' ';'
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#iteration_statement>
    fn do_while_statement(&mut self) -> StmtResult {
        let start = self
            .expect(TokenKind::Keyword(Keyword::Do))
            .unwrap_or_else(|_| {
                panic!("do_while_statement should only be called with `do` as next token")
            });
        let body = self.statement()?;
        self.expect(TokenKind::Keyword(Keyword::While))?;
        self.expect(TokenKind::LeftParen)?;
        let condition = self.expr()?;
        self.expect(TokenKind::RightParen)?;
        self.expect(TokenKind::Semicolon)?;
        let stmt = StmtType::Do(Box::new(body), condition);
        Ok(Stmt {
            data: stmt,
            location: start.location,
        })
    }

    /// `expr_opt: expr ';' | ';'`
    ///
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#expression_statement>
    ///
    /// `token` is the delimiter that ends the expression;
    /// `token` is usually `;` but sometimes `)` (in `for` loops)
    pub(super) fn expr_opt(&mut self, token: TokenKind) -> SyntaxResult<Option<Expr>> {
        if self.match_next(&token).is_some() {
            Ok(None)
        } else {
            let expr = self.expr()?;
            self.expect(token)?;
            Ok(Some(expr))
        }
    }

    /// ```yacc
    /// for_statement:
    ///     FOR '(' expr_opt ';' expr_opt ';' expr_opt ') statement
    ///   | FOR '(' declaration expr_opt ';' expr_opt ') statement;
    /// ```
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#iteration_statement>
    fn for_statement(&mut self) -> StmtResult {
        let start = self.expect(TokenKind::Keyword(Keyword::For))?;
        let paren = self.expect(TokenKind::LeftParen)?;
        let expr_opt = |this: &mut Self| {
            let expr = this.expr_opt(TokenKind::Semicolon)?;
            let semicolon = this.last_location;
            Ok(if let Some(expr) = expr {
                let location = expr.location.maybe_merge(semicolon);
                Stmt::new(StmtType::Expr(expr), location)
            } else {
                Stmt::new(StmtType::default(), semicolon)
            })
        };
        let decl_stmt = match self.peek_token() {
            Some(TokenKind::Keyword(k)) if k.is_decl_specifier() => self.declaration()?,
            Some(TokenKind::Identifier(id)) => {
                let id = *id;
                if self.typedefs.get(&id).is_some() {
                    self.declaration()?
                } else {
                    expr_opt(self)?
                }
            }
            Some(_) => expr_opt(self)?,
            None => {
                return Err(self
                    .last_location
                    .with(SyntaxError::EndOfFile("expression or ';'")));
            }
        };
        let initializer = Box::new(Stmt {
            data: decl_stmt.data,
            location: paren.location.maybe_merge(decl_stmt.location),
        });
        let controlling_expr = self.expr_opt(TokenKind::Semicolon)?;
        let iter_expr = self.expr_opt(TokenKind::RightParen)?;
        let body = Box::new(self.statement()?);
        Ok(Stmt {
            data: StmtType::For {
                initializer,
                condition: controlling_expr.map(Box::new),
                post_loop: iter_expr.map(Box::new),
                body,
            },
            location: start.location,
        })
    }
    /// `goto_statement: GOTO identifier ';'`
    ///
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#jump_statement>
    fn goto_statement(&mut self) -> StmtResult {
        let start = self.expect(TokenKind::Keyword(Keyword::Goto)).unwrap();
        let id = self.expect_id()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt {
            data: StmtType::Goto(id.data),
            location: start.location.maybe_merge(id.location),
        })
    }
}

impl ExternalDeclaration {
    /// If this is a `Declaration`, return all declarations seen.
    /// Otherwise, return the declarator for the function definition.
    fn into_declaration(self) -> Result<Declaration, SyntaxError> {
        match self {
            ExternalDeclaration::Function(def) => Err(SyntaxError::FunctionNotAllowed(def)),
            ExternalDeclaration::Declaration(decl) => Ok(decl),
        }
    }
}
