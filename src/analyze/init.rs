//! 6.7.9 Initialization

use crate::analyze::error::SemanticError;
use crate::analyze::hir::{Expr, ExprType, Initializer, TypeKind};
use crate::location::Location;
use crate::parse::ast;

use super::PureAnalyzer;

impl PureAnalyzer {
    pub(super) fn parse_initializer(
        &mut self,
        init: ast::Initializer,
        ctype: &TypeKind,
        location: Location,
    ) -> Initializer {
        use ast::Initializer::{Aggregate, Scalar};
        // initializer_list
        let mut expr = match init {
            Aggregate(list) => return self.check_aggregate_overflow(list, ctype, location),
            Scalar(expr) => self.expr(*expr),
        };
        // The only time (that I know of) that an expression will initialize a non-scalar
        // is for character literals.
        let is_char_array = match ctype {
            TypeKind::Array(inner, _) => inner.is_char(),
            _ => false,
        };
        // See section 6.7.9 of the C11 standard:
        // The initializer for a scalar shall be a single expression, optionally enclosed in braces.
        // The initial value of the object is that of the expression (after conversion)
        if !is_char_array {
            expr = expr
                .rval()
                // if ctype is not a scalar, this will report an error, so we don't have to handle it specially
                .implicit_cast(ctype, &mut self.error_handler);
        }
        if !expr.lval && self.scope.is_global() && ctype.is_pointer() {
            expr = Expr {
                lval: false,
                location: expr.location,
                ctype: expr.ctype.clone(),
                expr: ExprType::StaticRef(Box::new(expr)),
            };
        }
        Initializer::Scalar(Box::new(expr))
    }

    fn check_aggregate_overflow(
        &mut self,
        list: Vec<ast::Initializer>,
        ctype: &TypeKind,
        location: Location,
    ) -> Initializer {
        let len = list.len();
        let mut iter = list.into_iter().peekable();
        let init = self.aggregate_initializer(&mut iter, ctype, location);
        let leftover = iter.count();
        if leftover > 0 {
            self.err(SemanticError::TooManyMembers(len - leftover, len), location);
        }
        init
    }

    // handle char[][3] = {{1,2,3}}, but also = {1,2,3} and {{1}, 2, 3}
    // NOTE: this does NOT consume {} except for sub-elements
    // see p17: "Each brace-enclosed initializer list has an associated current object"
    // For each subobject of the enclosing object (`type_at`), initialize it, possibly recursively.
    fn aggregate_initializer(
        &mut self,
        list: &mut std::iter::Peekable<impl Iterator<Item = ast::Initializer>>,
        elem_type: &TypeKind,
        location: Location,
    ) -> Initializer {
        use ast::Initializer::{Aggregate, Scalar};

        let mut elems = vec![];
        if list.peek().is_none() {
            self.err(SemanticError::EmptyInitializer, location);
            return Initializer::InitializerList(elems);
        }
        // char [][3] = {1};
        while let Some(elem) = list.peek() {
            let inner = elem_type.type_at(elems.len()).unwrap_or_else(|err| {
                // int a[1] = {1, 2};
                self.err(err, location);
                TypeKind::Error
            });
            // int a[][3] = {{1,2,3}}
            //               ^
            // initializer is aggregate, type errors will be caught later

            // If the initializer of a subaggregate or contained union begins with a left brace,
            // the initializers enclosed by that brace and its matching right brace initialize
            // the elements or members of the subaggregate or the contained union.
            let next = match elem {
                Aggregate(_) => match list.next() {
                    Some(Aggregate(inner_list)) => {
                        self.check_aggregate_overflow(inner_list, &inner, location)
                    }
                    _ => unreachable!(),
                },
                Scalar(_) => {
                    // int a[][3] = {1,2,3}
                    //               ^
                    // type is aggregate and initializer is scalar
                    // see if we can short circuit int[][3] -> int[3]
                    if inner != TypeKind::Error && !inner.is_scalar() {
                        // Note: this element is _not_ consumed
                        self.aggregate_initializer(list, &inner, location)
                    // type is scalar and initializer is scalar
                    // int a[][3] = {{1,2,3}}
                    } else {
                        let expr = match list.next() {
                            Some(Scalar(expr)) => self
                                .expr(*expr)
                                .rval()
                                .implicit_cast(&inner, &mut self.error_handler),
                            _ => unreachable!(),
                        };
                        Initializer::Scalar(Box::new(expr))
                    }
                }
            };
            elems.push(next);

            // Otherwise, only enough initializers from the list are taken
            // to account for the elements or members of the subaggregate
            // or the first member of the contained union;
            // any remaining initializers are left to initialize the next
            // element or member of the aggregate of which the current
            // subaggregate or contained union is a part.
            if elems.len() == elem_type.type_len() {
                break;
            }
        }
        Initializer::InitializerList(elems)
    }
}

impl TypeKind {
    /// Given a type, return the maximum number of initializers for that type
    fn type_len(&self) -> usize {
        use crate::analyze::hir::ArrayType;

        match self {
            ty if ty.is_scalar() => 1,
            TypeKind::Array(_, ArrayType::Fixed(size)) => *size as usize,
            TypeKind::Array(_, ArrayType::Unbounded) => 0,
            TypeKind::Struct(st) | TypeKind::Union(st) => st.members().len(),
            TypeKind::Function { .. } | TypeKind::Error => 1,
            _ => unimplemented!("type checking for {}", self),
        }
    }

    /// Given a type and an index,
    /// return the type expected at that index in the initializer.
    ///
    /// e.g. if `struct s { int i; float f; };` is in scope,
    /// `type_at(s, 0)` will be `int` and `type_at(s, 1)` will be `float`
    fn type_at(&self, index: usize) -> Result<TypeKind, SemanticError> {
        match self {
            ty if ty.is_scalar() => {
                if index == 0 {
                    Ok(ty.clone())
                } else {
                    Err(SemanticError::AggregateInitializingScalar(
                        ty.clone(),
                        index + 1,
                    ))
                }
            }
            TypeKind::Array(inner, _) => Ok((**inner).clone()),
            TypeKind::Struct(struct_type) => {
                let symbols = struct_type.members();
                symbols.get(index).map_or_else(
                    || Err(SemanticError::TooManyMembers(symbols.len(), index)),
                    |symbol| Ok(symbol.ctype.clone()),
                )
            }
            TypeKind::Union(struct_type) => {
                if index != 0 {
                    return Err("can only initialize first member of an enum".into());
                }
                let members = struct_type.members();
                Ok(members
                    .first()
                    .map(|m| m.ctype.clone())
                    .unwrap_or(TypeKind::Error))
            }
            TypeKind::Function { .. } | TypeKind::Error => Ok(TypeKind::Error),
            _ => unimplemented!("type checking for aggregate initializers of type {}", self),
        }
    }
}
