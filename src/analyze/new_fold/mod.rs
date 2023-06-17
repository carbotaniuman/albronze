#[cfg(test)]
mod tests;

use crate::data::{ComparisonToken, LiteralValue};
use crate::hir::{BinaryOp, Expr, ExprType, TypeKind};

use crate::location::{Locatable, Location};
use dyn_safe::dyn_safe;
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum FoldError {
    #[error("{} overflow in expression", if *(.is_positive) { "positive" } else { "negative" })]
    ConstOverflow { is_positive: bool },

    #[error("cannot divide by zero")]
    DivideByZero,

    #[error("cannot shift {} by a negative amount", if *(.is_left) { "left" } else { "right" })]
    NegativeShift { is_left: bool },

    #[error("cannot shift {} by {maximum} or more bits for type '{ctype}' (got {current})",
        if *(.is_left) { "left" } else { "right" })]
    TooManyShiftBits {
        is_left: bool,
        maximum: u64,
        ctype: TypeKind,
        current: u64,
    },

    #[error("the given expression is invalid in this context")]
    Invalid,
}

/// Trait for a common interface for all constant expression evaluators.
#[dyn_safe(true)]
pub trait Folder {
    /// Folds the given `expr` into a single [`Expr`] with type
    /// [`ExprType::Literal`], or errors. This funtion returns the original
    /// expression in case of failure.
    fn const_fold(&self, expr: &Expr) -> Result<Expr, Locatable<FoldError>>;
}

impl<T: Folder> Folder for &T {
    fn const_fold(&self, expr: &Expr) -> Result<Expr, Locatable<FoldError>> {
        (*self).const_fold(expr)
    }
}

pub struct NoopFolder;

impl Folder for NoopFolder {
    fn const_fold(&self, expr: &Expr) -> Result<Expr, Locatable<FoldError>> {
        Err(expr.location.with(FoldError::Invalid))
    }
}

pub struct PreprocessorFolder;

impl Folder for PreprocessorFolder {
    fn const_fold(&self, expr: &Expr) -> Result<Expr, Locatable<FoldError>> {
        let location = expr.location;

        use LiteralValue::*;
        let folded = match &expr.expr {
            a @ ExprType::Literal(Int(_) | Char(_)) => a.clone(),
            ExprType::Literal(Float(_) | String(_)) => {
                return Err(expr.location.with(FoldError::Invalid))
            }
            ExprType::Negate(inner) => {
                map_literal(self, inner, location, |literal| match literal {
                    Int(i) => {
                        let (value, overflowed) = i.overflowing_neg();
                        if overflowed {
                            Err(FoldError::ConstOverflow { is_positive: i > 0 })
                        } else {
                            Ok(Int(value))
                        }
                    }
                    UnsignedInt(u) => {
                        // let signed = i64::try_from(big_number).map_err(|_| )?;
                        todo!()
                    }
                    Char(c) => Ok(Char(c.wrapping_neg())),
                    _ => unreachable!(),
                })?
            }
            ExprType::BitwiseNot(inner) => {
                map_literal(self, inner, location, |literal| match literal {
                    Int(i) => Ok(Int(!i)),
                    UnsignedInt(u) => Ok(UnsignedInt(!u)),
                    Char(c) => Ok(Char(!c)),
                    _ => unreachable!("bitwise not on invalid type"),
                })?
            }
            ExprType::Binary(op, left, right) => fold_binary(self, &left, &right, *op, location)?,
            ExprType::Ternary(condition, then, otherwise) => {
                let condition = self.const_fold(&condition)?;

                match condition.expr {
                    ExprType::Literal(Int(0) | UnsignedInt(0)) => self.const_fold(&otherwise)?.expr,
                    ExprType::Literal(Int(_) | UnsignedInt(_)) => self.const_fold(&then)?.expr,
                    _ => unreachable!(),
                }
            }
            ExprType::Comma(..) | ExprType::FuncCall(..) | ExprType::Stmt(..) => {
                return Err(expr.location.with(FoldError::Invalid))
            }
            ExprType::Cast(inner) if expr.ctype == TypeKind::Bool => bool_cast(self, inner)?,
            d => unreachable!("{:?} should have been caught by preprocessor", d),
        };

        Ok(Expr {
            expr: folded,
            ctype: expr.ctype.clone(),
            lval: expr.lval,
            location,
        })
    }
}

fn handle_bin_op<M>(left: &ExprType, right: &ExprType, mapper: M) -> Result<ExprType, FoldError>
where
    M: FnOnce(&LiteralValue, &LiteralValue) -> Result<LiteralValue, FoldError>,
{
    let left = match left {
        ExprType::Literal(literal) => literal,
        _ => unreachable!(),
    };
    let right = match right {
        ExprType::Literal(literal) => literal,
        _ => unreachable!(),
    };

    Ok(ExprType::Literal(mapper(left, right)?))
}

macro_rules! fold_int_bin_op {
    ($op: tt) => {
        |a: &LiteralValue, b: &LiteralValue| {
            use LiteralValue::*;
            match (a, b) {
                (Int(a), Int(b)) => Ok(Int(a $op b)),
                (UnsignedInt(a), UnsignedInt(b)) => Ok(UnsignedInt(a $op b)),
                (Char(a), Char(b)) => Ok(Char(a $op b)),
                (_, _) => todo!(),
            }
        }
    }
}

macro_rules! fold_compare_op {
    ($left: expr, $right: expr, $constructor: ident, $op: tt) => {{
            use LiteralValue::*;

            let left: &ExprType = $left;
            let right: &ExprType = $right;
            match (left, right) {
                (ExprType::Literal(a), ExprType::Literal(b)) => {
                    match (a, b) {
                        (Int(a), Int(b)) => ExprType::Literal(Int((a $op b) as i64)),
                        (UnsignedInt(a), UnsignedInt(b)) => ExprType::Literal(Int((a $op b) as i64)),
                        #[allow(clippy::float_cmp)]
                        (Float(a), Float(b)) => ExprType::Literal(Int((a $op b) as i64)),
                        (Char(a), Char(b)) => ExprType::Literal(Int((a $op b) as i64)),
                        (_, _) => todo!(),
                    }
                }
                _ => todo!(),
            }
        }}
    }

fn fold_binary(
    folder: impl Folder,
    left: &Expr,
    right: &Expr,
    op: BinaryOp,
    location: Location,
) -> Result<ExprType, Locatable<FoldError>> {
    let left = folder.const_fold(left)?;
    let right = folder.const_fold(right)?;

    use std::ops::*;
    use BinaryOp::*;
    use ComparisonToken::*;
    let expr_type = match op {
        Add => handle_bin_op(
            &left.expr,
            &right.expr,
            fold_scalar_bin_op(
                f64::add,
                i64::overflowing_add,
                u64::wrapping_add,
                u8::wrapping_add,
            ),
        ),
        Sub => handle_bin_op(
            &left.expr,
            &right.expr,
            fold_scalar_bin_op(
                f64::sub,
                i64::overflowing_sub,
                u64::wrapping_sub,
                u8::wrapping_sub,
            ),
        ),
        Mul => handle_bin_op(
            &left.expr,
            &right.expr,
            fold_scalar_bin_op(
                f64::mul,
                i64::overflowing_mul,
                u64::wrapping_mul,
                u8::wrapping_mul,
            ),
        ),
        Div => {
            if right.ctype.is_integral() && right.is_zero() {
                return Err(location.with(FoldError::DivideByZero));
            }
            handle_bin_op(
                &left.expr,
                &right.expr,
                fold_scalar_bin_op(
                    f64::div,
                    i64::overflowing_div,
                    u64::wrapping_div,
                    u8::wrapping_div,
                ),
            )
        }
        Mod => {
            if right.is_zero() {
                return Err(location.with(FoldError::DivideByZero));
            }

            use LiteralValue::*;
            handle_bin_op(
                &left.expr,
                &right.expr,
                |a: &LiteralValue, b: &LiteralValue| match (a, b) {
                    (Int(a), Int(b)) => {
                        let (value, overflowed) = a.overflowing_rem(*b);

                        if overflowed {
                            Err(FoldError::ConstOverflow {
                                is_positive: value.is_negative(),
                            })
                        } else {
                            Ok(Int(value))
                        }
                    }
                    (UnsignedInt(a), UnsignedInt(b)) => Ok(UnsignedInt(a.wrapping_rem(*b))),
                    (_, _) => unreachable!(),
                },
            )
        }
        Xor => handle_bin_op(&left.expr, &right.expr, fold_int_bin_op!(^)),
        BitwiseAnd => handle_bin_op(&left.expr, &right.expr, fold_int_bin_op!(&)),
        BitwiseOr => handle_bin_op(&left.expr, &right.expr, fold_int_bin_op!(|)),
        //     Shl => shift_left(left, right, parent_type, &location),
        //     Shr => shift_right(left, right, parent_type, &location),
        LogicalAnd => handle_bin_op(&left.expr, &right.expr, |left, right| {
            use LiteralValue::*;
            match (left, right) {
                (Int(1), Int(1)) => Ok(Int(1)),
                (Int(0), _) | (_, Int(0)) => Ok(Int(0)),
                _ => todo!(),
            }
        }),
        LogicalOr => handle_bin_op(&left.expr, &right.expr, |left, right| {
            use LiteralValue::*;
            match (left, right) {
                (Int(0), Int(0)) => Ok(Int(0)),
                (Int(1), _) | (_, Int(1)) => Ok(Int(1)),
                _ => todo!(),
            }
        }),
        Assign => Err(FoldError::Invalid),
        Compare(Less) => Ok(fold_compare_op!(&left.expr, &right.expr, Compare, <)),
        Compare(LessEqual) => Ok(fold_compare_op!(&left.expr, &right.expr, Compare, <=)),
        Compare(Greater) => Ok(fold_compare_op!(&left.expr, &right.expr, Compare, >)),
        Compare(GreaterEqual) => Ok(fold_compare_op!(&left.expr, &right.expr, Compare, >=)),
        Compare(EqualEqual) => Ok(fold_compare_op!(&left.expr, &right.expr, Compare, ==)),
        Compare(NotEqual) => Ok(fold_compare_op!(&left.expr, &right.expr, Compare, !=)),
        _ => todo!(),
    }
    .map_err(|e| location.with(e))?;

    Ok(expr_type)
}

#[inline]
fn fold_scalar_bin_op(
    simple: fn(f64, f64) -> f64,
    overflowing: fn(i64, i64) -> (i64, bool),
    wrapping: fn(u64, u64) -> u64,
    wrapping_byte: fn(u8, u8) -> u8,
) -> impl Fn(&LiteralValue, &LiteralValue) -> Result<LiteralValue, FoldError> {
    use LiteralValue::*;
    move |a: &LiteralValue, b: &LiteralValue| match (a, b) {
        (Int(a), Int(b)) => {
            // overflowing returns the wrapped value, so if we had a negative
            // value, it would be a positive overflow.
            let (value, overflowed) = overflowing(*a, *b);
            if overflowed {
                Err(FoldError::ConstOverflow {
                    is_positive: value.is_negative(),
                })
            } else {
                Ok(Int(value))
            }
        }
        (UnsignedInt(a), UnsignedInt(b)) => Ok(UnsignedInt(wrapping(*a, *b))),
        (Char(a), Char(b)) => Ok(Char(wrapping_byte(*a, *b))),
        (Float(a), Float(b)) => Ok(Float(simple(*a, *b))),
        // TODO: find a way to do this that allows `"hello" + 2 - 1`
        //(Str(s), Int(i)) | (Int(i), Str(s)) => {
        _ => todo!(),
    }
}

fn bool_cast(folder: impl Folder, expr: &Expr) -> Result<ExprType, Locatable<FoldError>> {
    let mut expr = folder.const_fold(expr)?;

    use LiteralValue::*;
    Ok(
        match expr.expr {
            ExprType::Literal(Int(i)) => ExprType::Literal(Int((i != 0) as i64)),
            ExprType::Literal(UnsignedInt(i)) => ExprType::Literal(UnsignedInt((i != 0) as u64)),
            _ => unreachable!(),
        }
    )
}

// fn cast(expr: Expr, ctype: &TypeKind) -> CompileResult<ExprType> {
//     let expr = expr.const_fold()?;
//     Ok(if let ExprType::Literal(ref token) = expr.expr {
//         if let Some(token) = const_cast(token, ctype) {
//             ExprType::Literal(token)
//         } else {
//             ExprType::Cast(Box::new(expr))
//         }
//     } else {
//         ExprType::Cast(Box::new(expr))
//     })
// }

// /// since we only have Int and Float for literals,
// /// all this does is make sure the folded value is in a valid range
// /// TODO: when we add suffix literals, that will have type information
// /// and we can use that to store the new type
// fn const_cast(token: &LiteralValue, ctype: &TypeKind) -> Option<LiteralValue> {
//     use LiteralValue::*;
//     let token = match (token, ctype) {
//         (Int(i), TypeKind::Bool) => Int((*i != 0).into()),
//         (Int(i), TypeKind::Char(_)) => Char(*i as u8),
//         (Int(i), TypeKind::Double) | (Int(i), TypeKind::Float) => Float(*i as f64),
//         (Int(i), ty) if ty.is_integral() && ty.is_signed() => Int(*i),
//         (Int(i), ty) if ty.is_integral() => UnsignedInt(*i as u64),

//         (UnsignedInt(u), TypeKind::Bool) => Int((*u != 0).into()),
//         (UnsignedInt(u), TypeKind::Char(_)) => Char(*u as u8),
//         (UnsignedInt(u), TypeKind::Double) | (UnsignedInt(u), TypeKind::Float) => Float(*u as f64),
//         (UnsignedInt(u), ty) if ty.is_integral() && ty.is_signed() => Int(*u as i64),
//         (UnsignedInt(u), ty) if ty.is_integral() => UnsignedInt(*u),

//         (Float(f), TypeKind::Bool) => Int((*f != 0.0) as i64),
//         (Float(f), TypeKind::Char(_)) => Char(*f as u8),
//         (Float(f), TypeKind::Double) | (Float(f), TypeKind::Float) => Float(*f),
//         (Float(f), ty) if ty.is_integral() && ty.is_signed() => Int(*f as i64),
//         (Float(f), ty) if ty.is_integral() => UnsignedInt(*f as u64),

//         (&Char(c), TypeKind::Bool) => Int((c != 0).into()),
//         (&Char(c), TypeKind::Double) | (&Char(c), TypeKind::Float) => Float(c.into()),
//         (&Char(c), ty) if ty.is_integral() && ty.is_signed() => Int(c.into()),
//         (&Char(c), ty) if ty.is_integral() => UnsignedInt(c.into()),

//         (Int(i), _) if ctype.is_pointer() && *i >= 0 => UnsignedInt(*i as u64),
//         (UnsignedInt(u), _) if ctype.is_pointer() => UnsignedInt(*u),
//         (&Char(c), _) if ctype.is_pointer() => UnsignedInt(c.into()),
//         _ => return None,
//     };
//     Some(token)
// }

fn map_literal<M>(
    folder: impl Folder,
    inner: &Expr,
    location: Location,
    mapper: M,
) -> Result<ExprType, Locatable<FoldError>>
where
    M: FnOnce(LiteralValue) -> Result<LiteralValue, FoldError>,
{
    let folded = folder.const_fold(inner)?;
    let literal = match folded.expr {
        ExprType::Literal(literal) => mapper(literal).map_err(|e| location.with(e))?,
        _ => unreachable!(),
    };

    Ok(ExprType::Literal(literal))
}
