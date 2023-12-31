use crate::analyze::error::SemanticError;
use crate::analyze::FunctionAnalyzer;
use crate::data::LiteralValue;
use crate::hir::{Expr, Stmt, StmtType, TypeKind};
use crate::location::{Locatable, Location};
use crate::parse::ast;

impl FunctionAnalyzer<'_> {
    #[inline(always)]
    fn expr(&mut self, expr: ast::Expr) -> Expr {
        self.analyzer.expr(expr)
    }

    pub(crate) fn parse_stmt(&mut self, stmt: ast::Stmt) -> Stmt {
        use ast::StmtType::*;
        use StmtType as S;

        // ugh so much boilerplate
        let data = match stmt.data {
            Compound(stmts) => {
                // 6.2.1 Scopes of identifiers
                self.enter_scope();
                let mut parsed = Vec::new();
                for inner in stmts {
                    parsed.push(self.parse_stmt(inner));
                }
                self.leave_scope(stmt.location);
                S::Compound(parsed)
            }
            // 6.8.3 Expression and null statements
            Expr(expr) => S::Expr(self.expr(expr)),
            // 6.8.4.1 The if statement
            If(condition, then, otherwise) => {
                let condition = self
                    .expr(condition)
                    .truthy(&mut self.analyzer.error_handler);
                let then = self.parse_stmt(*then);
                let otherwise = otherwise.map(|s| Box::new(self.parse_stmt(*s)));
                S::If(condition, Box::new(then), otherwise)
            }
            // 6.8.4.2 The switch statement
            Switch(value, body) => {
                let value = self.expr(value).rval();
                if !value.ctype.is_integral() {
                    self.err(
                        SemanticError::NonIntegralSwitch(value.ctype.clone()),
                        stmt.location,
                    )
                }
                let body = self.parse_stmt(*body);
                S::Switch(value, Box::new(body))
            }
            // 6.8.5.2 The do statement
            Do(body, condition) => {
                let body = self.parse_stmt(*body);
                let condition = self
                    .expr(condition)
                    .truthy(&mut self.analyzer.error_handler);
                S::Do(Box::new(body), condition)
            }
            // 6.8.5.1 The while statement
            While(condition, body) => {
                let condition = self
                    .expr(condition)
                    .truthy(&mut self.analyzer.error_handler);
                let body = self.parse_stmt(*body);
                S::While(condition, Box::new(body))
            }
            // 6.8.5.3 The for statement
            For {
                initializer,
                condition,
                post_loop,
                body,
            } => {
                // TODO: maybe a sanity check here that the init statement is only an expression or declaration?
                // Or encode that in the type somehow?
                self.enter_scope();
                let initializer = self.parse_stmt(*initializer);
                let condition = condition
                    .map(|e| Box::new(self.expr(*e).truthy(&mut self.analyzer.error_handler)));
                let post_loop = post_loop.map(|e| Box::new(self.expr(*e)));
                let body = self.parse_stmt(*body);
                self.leave_scope(stmt.location);
                S::For(Box::new(initializer), condition, post_loop, Box::new(body))
            }
            // 6.8.1 Labeled statements
            // TODO: all of these should have semantic checking here, not in the backend
            Label(name, inner) => {
                let inner = self.parse_stmt(*inner);
                S::Label(name, Box::new(inner))
            }
            Case(expr, inner) => self.case_statement(*expr, *inner, stmt.location),
            // 6.8.1 Labeled statements
            Default(inner) => S::Default(Box::new(self.parse_stmt(*inner))),
            // 6.8.6.1 The goto statement
            Goto(label) => S::Goto(label),
            // 6.8.6.2 The continue statement
            Continue => S::Continue,
            // 6.8.6.3 The break statement
            Break => S::Break,
            Return(value) => self.return_statement(value, stmt.location),
            // 6.7 Declarations
            Decl(decls) => S::Decl(self.analyzer.parse_declaration(decls, stmt.location)),
        };

        Locatable::new(data, stmt.location)
    }
    // 6.8.1 Labeled statements
    fn case_statement(
        &mut self,
        expr: ast::Expr,
        inner: ast::Stmt,
        location: Location,
    ) -> StmtType {
        use super::expr::literal;

        let expr = match self.expr(expr).const_fold() {
            Ok(e) => e,
            Err(err) => {
                self.analyzer.error_handler.push_error(err);
                Expr::zero(location)
            }
        };
        let int = match expr.into_literal() {
            Ok(LiteralValue::Int(i)) => i as u64,
            Ok(LiteralValue::UnsignedInt(u)) => u,
            Ok(LiteralValue::Char(c)) => c.into(),
            Ok(other) => {
                let ctype = literal(other, location).ctype;
                self.err(SemanticError::NonIntegralExpr(ctype), location);
                0
            }
            Err(other) => {
                self.err(SemanticError::NotConstant(other), location);
                0
            }
        };
        let inner = self.parse_stmt(inner);
        StmtType::Case(int, Box::new(inner))
    }
    // 6.8.6.4 The return statement
    // A value of `None` for `expr` means `return;`
    fn return_statement(&mut self, expr: Option<ast::Expr>, location: Location) -> StmtType {
        let expr = expr.map(|e| self.expr(e));
        let ret_type = &self.metadata.return_type;
        match (expr, *ret_type != TypeKind::Void) {
            // void f() { return ;}
            (None, false) => StmtType::Return(None),
            // int f() { return; }
            (None, true) => {
                self.err(
                    SemanticError::MissingReturnValue(self.metadata.id),
                    location,
                );
                StmtType::Return(None)
            }
            // void f() { return 1; }
            (Some(expr), false) => {
                self.err(
                    SemanticError::ReturnFromVoid(self.metadata.id),
                    expr.location,
                );
                StmtType::Return(None)
            }
            // int f() { return 1; }
            (Some(expr), true) => {
                let expr = expr.rval();
                if expr.ctype != *ret_type {
                    StmtType::Return(Some(
                        expr.implicit_cast(ret_type, &mut self.analyzer.error_handler),
                    ))
                } else {
                    StmtType::Return(Some(expr))
                }
            }
        }
    }
}
