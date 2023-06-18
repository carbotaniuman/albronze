use super::*;
use crate::parse::ast::{FloatRadix, LiteralData, IntegerRadix};

use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum NumericLiteralError {
    #[error("hexadecimal floating point literals require an exponent")]
    MissingHexExponent,

    #[error("exponent is missing digits")]
    EmptyExponent,
}

pub fn number_literal(value: &str) -> Result<LiteralData, NumericLiteralError> {
    // So, integer literals in C are actually pretty complicated, given
    // hexadecimal, integral, and octal. We do parsing in a linear
    // scan fashion here.

    let is_hexdigit = |f: char| f.is_ascii_hexdigit() || f == '.';
    let is_digit = |f: char| f.is_ascii_digit() || f == '.';

    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    enum Mode {
        Hex,
        Oct,
        Dec,
    }

    impl Mode {
        fn chars_to_skip(self) -> usize {
            match self {
                Mode::Hex => 2,
                Mode::Oct => 1,
                Mode::Dec => 0,
            }
        }

        fn to_integer_radix(self) -> IntegerRadix {
            match self {
                Mode::Hex => IntegerRadix::Hexadecimal,
                Mode::Oct => IntegerRadix::Octal,
                Mode::Dec => IntegerRadix::Decimal,
            }
        }

        fn to_float_radix(self) -> FloatRadix {
            match self {
                Mode::Hex => FloatRadix::Hexadecimal,
                Mode::Dec => FloatRadix::Decimal,
                Mode::Oct => unreachable!(),
            }
        }
    }

    // Figure out what kind of 
    let mut mode = if value.starts_with("0x") || value.starts_with("0X") {
        Mode::Hex
    } else if value.starts_with("0") {
        Mode::Oct
    } else {
        Mode::Dec
    };

    // This (along with `mode` above), may get changed depending on
    // what the rest of the scan reveals. For instance, a lone `0` is `0`,
    // not an empty octal constant, while `0.7` or `0.` are float literals. 
    let mut rest = &value[mode.chars_to_skip()..];

    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    enum State {
        Integer,
        Fraction,
        Exponent,
        Suffix,
    }

    // finds the end of the integers, but is `None` if `rest` has only integers
    // remaining, so it's really the start of the fraction, exponent, or suffix.
    let non_integer_start = match mode {
        Mode::Hex => rest.find(|x: char| !x.is_ascii_hexdigit()),
        Mode::Oct if rest.is_empty() => {
            // if `rest` is empty, then we parsed a 0 as an octal,
            // instead of a plain 0. fix that here
            mode = Mode::Dec;
            rest = value;
            None
        },
        Mode::Oct => rest.find(|x: char| !x.is_ascii_digit()),
        Mode::Dec => rest.find(|x: char| !x.is_ascii_digit()),
    };

    // after this, we know that numbers must have a fraction,
    // exponent, or suffix
    let Some(mut non_integer_start) = non_integer_start else {
        return Ok(LiteralData::Integer {
            value: rest.into(),
            suffix: InternedStr::default(),
            radix: mode.to_integer_radix(),
        });
    };

    // from here on out, we'll just reuse state and rest,
    // and reassign them to deal with all the possible conditions
    // that may occur.
    let (is_float, integer, mut rest, mut state) = 'n: {
                let remainder = &rest[non_integer_start..];

        let new_state = if remainder.starts_with(".") {
            State::Fraction
        } else if remainder.starts_with("p") || remainder.starts_with("P") {
            debug_assert_eq!(mode, Mode::Hex);
            State::Exponent
        } else if remainder.starts_with("e") || remainder.starts_with("E") {
            debug_assert_ne!(mode, Mode::Hex);
            State::Exponent
        } else {
            // We are not a float, we don't need to do any fixups
            break 'n (false, &rest[0..non_integer_start], remainder, State::Suffix)
        };

        if mode == Mode::Oct {
            mode = Mode::Dec;
            rest = value;
            non_integer_start += 1;
        };
        (true, &rest[0..non_integer_start], remainder, new_state)
    };

    let fraction = if state == State::Fraction {
        let fraction_part = &rest[1..];
        let non_fraction_start = match mode {
            Mode::Hex => fraction_part.find(|x: char| !x.is_ascii_hexdigit()),
            Mode::Dec => fraction_part.find(|x: char| !x.is_ascii_digit()),
            Mode::Oct => unreachable!(),
        };

        let Some(non_fraction_start) = non_fraction_start else {
            if mode == Mode::Hex {
                return Err(NumericLiteralError::MissingHexExponent);
            }

            return Ok(LiteralData::Float {
                significand: integer.into(),
                fraction: fraction_part.into(),
                exponent: InternedStr::default(),
                suffix: InternedStr::default(),
                radix: mode.to_float_radix(),
            });
        };
        
        // advance the state to exponent or suffix
        rest = &fraction_part[non_fraction_start..];
        if rest.starts_with("p") || rest.starts_with("P") {
            debug_assert_eq!(mode, Mode::Hex);
            state = State::Exponent;
        } else if rest.starts_with("e") || rest.starts_with("E") {
            debug_assert_eq!(mode, Mode::Dec);
            state = State::Exponent
        } else {
            if mode == Mode::Hex {
                return Err(NumericLiteralError::MissingHexExponent);
            }

            state = State::Suffix
        }

        &fraction_part[0..non_fraction_start]
    } else {
        ""
    };

    let exponent = if state == State::Exponent {
        let exponent_part = &rest[1..];
        let non_exponent_start = exponent_part.find(|x: char| !x.is_ascii_digit());

        let Some(non_exponent_start) = non_exponent_start else {
            if exponent_part.is_empty() {
                return Err(NumericLiteralError::EmptyExponent);
            } else {
                return Ok(LiteralData::Float {
                    significand: integer.into(),
                    fraction: fraction.into(),
                    exponent: exponent_part.into(),
                    suffix: InternedStr::default(),
                    radix: mode.to_float_radix(),
                });
            }
        };

        rest = &exponent_part[non_exponent_start..];
        state = State::Suffix;

        &exponent_part[0..non_exponent_start]
    } else {
        ""
    };

    let suffix = rest;

    if is_float {
        return Ok(LiteralData::Float {
            significand: integer.into(),
            fraction: fraction.into(),
            exponent: exponent.into(),
            suffix: suffix.into(),
            radix: mode.to_float_radix(),
        });
    } else {
        return Ok(LiteralData::Integer {
            value: integer.into(),
            suffix: suffix.into(),
            radix: mode.to_integer_radix(),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_literals() {
        assert_eq!(
            number_literal("100"),
            Ok(
                LiteralData::Integer {
                    value: "100".into(),
                    suffix: "".into(),
                    radix: IntegerRadix::Decimal,
                }
            )
        );

        assert_eq!(
            number_literal("0"),
            Ok(
                LiteralData::Integer {
                    value: "0".into(),
                    suffix: "".into(),
                    radix: IntegerRadix::Decimal,
                }
            )
        );

        assert_eq!(
            number_literal("01"),
            Ok(
                LiteralData::Integer {
                    value: "1".into(),
                    suffix: "".into(),
                    radix: IntegerRadix::Octal,
                }
            )
        );

        assert_eq!(
            number_literal("1abcde123"),
            Ok(
                LiteralData::Integer {
                    value: "1".into(),
                    suffix: "abcde123".into(),
                    radix: IntegerRadix::Decimal,
                }
            )
        );

        assert_eq!(
            number_literal("0x1abcde123!"),
            Ok(
                LiteralData::Integer {
                    value: "1abcde123".into(),
                    suffix: "!".into(),
                    radix: IntegerRadix::Hexadecimal,
                }
            )
        );
    }

    #[test]
    fn test_float_literals() {
        assert_eq!(
            number_literal("1.1"),
            Ok(
                LiteralData::Float {
                    significand: "1".into(),
                    fraction: "1".into(),
                    exponent: "".into(),
                    suffix: "".into(),
                    radix: FloatRadix::Decimal,
                }
            )
        );

        assert_eq!(
            number_literal("1.1e5"),
            Ok(
                LiteralData::Float {
                    significand: "1".into(),
                    fraction: "1".into(),
                    exponent: "5".into(),
                    suffix: "".into(),
                    radix: FloatRadix::Decimal,
                }
            )
        );

        assert_eq!(
            number_literal("1.1e5zzz"),
            Ok(
                LiteralData::Float {
                    significand: "1".into(),
                    fraction: "1".into(),
                    exponent: "5".into(),
                    suffix: "zzz".into(),
                    radix: FloatRadix::Decimal,
                }
            )
        );

        assert_eq!(
            number_literal("0x1.1p5"),
            Ok(
                LiteralData::Float {
                    significand: "1".into(),
                    fraction: "1".into(),
                    exponent: "5".into(),
                    suffix: "".into(),
                    radix: FloatRadix::Hexadecimal,
                }
            )
        );
    }

    #[test]
    fn test_float_literals_combo() {
        use std::iter::repeat;
        use itertools::Itertools;

        let iter = repeat([true, false].into_iter()).take(4).multi_cartesian_product();
        for vec in iter {
            let is_hex = vec[0];
            let has_fraction = vec[1];
            let has_exponent = vec[2];
            let has_suffix = vec[3];
            
            let sample = format!(
                "{}{}{}{}{}",
                if is_hex { "0x1" } else { "1" },
                if has_fraction { ".1" } else { "." },
                if has_exponent {
                    if is_hex { "p" } else { "e" }
                } else {
                    ""
                },
                if has_exponent { "5" } else { "" },
                if has_suffix { "asd" } else { "" },
            );

            insta::with_settings!({
                info => &sample, // the template context
                description => "", // the template source code
                omit_expression => true // do not include the default expression
            }, {
                insta::assert_debug_snapshot!(number_literal(&sample));
            });
        }
    }
}