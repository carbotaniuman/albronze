use crate::data::{AssignmentToken, ComparisonToken};
use crate::location::Locatable;
use crate::InternedStr;

use crate::data::{joined, INDENT};

use std::fmt;

use derive_more::From;

#[derive(Clone, Debug, PartialEq)]
pub enum ExternalDeclaration {
    Function(FunctionDefinition),
    Declaration(Declaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub id: InternedStr,
    pub declarator: FunctionDeclarator,
    pub body: CompoundStatement,
}

impl FunctionDefinition {
    pub(crate) fn as_type(&self) -> TypeName {
        TypeName {
            specifiers: self.specifiers.clone(),
            declarator: Declarator {
                decl: DeclaratorType::Function(self.declarator.clone()),
                id: Some(self.id),
            },
        }
    }
}

pub type Program = Vec<Declaration>;

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarators: Vec<Locatable<InitDeclarator>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeName {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarator: Declarator,
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq, From)]
pub enum DeclarationSpecifier {
    #[from]
    Unit(UnitSpecifier),
    Struct(StructSpecifier),
    Union(StructSpecifier),
    // enum name? { A = 1, B = 2, C }
    Enum {
        name: Option<InternedStr>,
        members: Option<Vec<(InternedStr, Option<Expr>)>>,
    },
    // NOTE: _not_ the same as UnitSpecifier::Typedef
    // that represents the `typedef` keyword, this represents a name that has been typedef-ed
    Typedef(InternedStr),
}

impl fmt::Display for DeclarationSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use DeclarationSpecifier::*;

        match self {
            Unit(u) => write!(f, "{}", u),
            Enum {
                name: Some(ident), ..
            } => write!(f, "enum {}", ident),
            // error, but caught later
            Enum {
                name: None,
                members: None,
            } => write!(f, "enum;"),
            Enum {
                name: None,
                members: Some(members),
            } => {
                let members = members.iter().map(|(name, value)| {
                    let val = if let Some(val) = value {
                        format!(" = {}", val)
                    } else {
                        String::new()
                    };
                    format!("{}{}", name, val)
                });
                write!(f, "enum {{ {} }}", joined(members, ", "))
            }
            Union(spec) => write!(f, "union {}", spec),
            Struct(spec) => write!(f, "struct {}", spec),
            Typedef(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnitSpecifier {
    // types
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Void,
    Signed,
    Unsigned,

    // weird types
    Bool,
    Complex,
    Imaginary,
    VaList,

    // qualifiers
    Const,
    Volatile,
    Restrict,
    // weird qualifiers
    Atomic,
    ThreadLocal,
    // function qualifiers
    Inline,
    NoReturn,

    // storage classes
    Auto,
    Register,
    Static,
    Extern,
    Typedef,
}

impl fmt::Display for UnitSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnitSpecifier::*;

        match self {
            Static => write!(f, "static"),
            Extern => write!(f, "extern"),
            Register => write!(f, "register"),
            Auto => write!(f, "auto"),
            Typedef => write!(f, "typedef"),

            Const => write!(f, "const"),
            Volatile => write!(f, "volatile"),
            Restrict => write!(f, "restrict"),
            Atomic => write!(f, "_Atomic"),
            ThreadLocal => write!(f, "_Thread_local"),

            Inline => write!(f, "inline"),
            NoReturn => write!(f, "_Noreturn"),

            Void => write!(f, "void"),
            Bool => write!(f, "_Bool"),
            Char => write!(f, "char"),
            Short => write!(f, "short"),
            Int => write!(f, "int"),
            Long => write!(f, "long"),
            Float => write!(f, "float"),
            Double => write!(f, "double"),
            Signed => write!(f, "signed"),
            Unsigned => write!(f, "unsigned"),

            Complex => write!(f, "_Complex"),
            Imaginary => write!(f, "_Imaginary"),
            VaList => write!(f, "va_list"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructSpecifier {
    pub name: Option<InternedStr>,
    /// Some([]): `struct s {}`
    /// None: `struct s;`
    pub members: Option<Vec<StructDeclarationList>>,
}

impl fmt::Display for StructSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ident) = self.name {
            write!(f, "{} ", ident)
        } else if let Some(body) = &self.members {
            writeln!(f, "{{")?;
            for decl in body {
                writeln!(f, "{}{}", INDENT, decl)?;
            }
            write!(f, "}}")
        } else {
            // what are we supposed to do for `struct;` lol
            todo!()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclarationList {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarators: Vec<StructDeclarator>,
}

impl fmt::Display for StructDeclarationList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ", joined(&self.specifiers, " "))?;
        write!(f, "{};", joined(&self.declarators, ", "))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclarator {
    /// optional since this could be only padding bits
    pub decl: Option<Declarator>,
    pub bitfield: Option<Expr>,
}

impl fmt::Display for StructDeclarator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(decl) = &self.decl {
            write!(f, "{}", decl)?;
        }
        if let Some(expr) = &self.bitfield {
            write!(f, ":{}", expr)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InitDeclarator {
    pub init: Option<Initializer>,
    pub declarator: Declarator,
}

impl fmt::Display for InitDeclarator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Initializer {
    Scalar(Box<Expr>),
    Aggregate(Vec<Initializer>),
}

impl fmt::Display for Initializer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarator {
    pub decl: DeclaratorType,
    pub id: Option<InternedStr>,
}

impl fmt::Display for Declarator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.decl.pretty_print(self.id, f)
    }
}

impl Declarator {
    fn is_nonempty(&self) -> bool {
        match self.decl {
            DeclaratorType::End => self.id.is_some(),
            _ => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclarator {
    pub return_type: Box<DeclaratorType>,
    // TODO: maybe support K&R C?
    //DeclarationList
    pub params: Vec<TypeName>,
    pub varargs: bool,
}

impl FunctionDeclarator {
    fn pretty_print(&self, name: Option<InternedStr>, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: maybe factor out some of the repeated code?
        // print_pre
        write!(f, "{}", self.return_type)?;
        // print_mid
        if let Some(name) = name {
            write!(f, "{}", name)?;
        }
        // print_post
        write!(f, "({}", joined(&self.params, ", "))?;
        if self.varargs {
            write!(f, ", ...")?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for FunctionDeclarator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.pretty_print(None, f)
    }
}

#[derive(Clone, Debug, PartialEq, From)]
pub enum DeclaratorType {
    // No more declarator, e.g. for abstract params
    End,
    Pointer {
        to: Box<DeclaratorType>,
        qualifiers: Vec<DeclarationSpecifier>,
    },
    Array {
        of: Box<DeclaratorType>,
        size: Option<Box<Expr>>,
    },
    #[from]
    Function(FunctionDeclarator),
}

impl DeclaratorType {
    fn pretty_print(&self, name: Option<InternedStr>, f: &mut fmt::Formatter) -> fmt::Result {
        let mut unrolled_type = Vec::new();
        let mut next_type = self;
        loop {
            unrolled_type.push(next_type);
            next_type = match next_type {
                DeclaratorType::Array { of: next, .. }
                | DeclaratorType::Pointer { to: next, .. }
                | DeclaratorType::Function(FunctionDeclarator {
                    return_type: next, ..
                }) => next.as_ref(),
                DeclaratorType::End => break,
            };
        }

        for declarator_type in unrolled_type[..unrolled_type.len() - 1].iter().rev() {
            match declarator_type {
                DeclaratorType::Pointer { qualifiers, .. } => {
                    write!(
                        f,
                        "(*{}",
                        qualifiers
                            .iter()
                            .map(|q| format!("{} ", q))
                            .collect::<Vec<_>>()
                            .concat()
                    )?;
                }
                DeclaratorType::Array { .. } | DeclaratorType::Function(_) => {}
                DeclaratorType::End => unreachable!(),
            }
        }
        if let Some(name) = name {
            write!(f, "{}", name)?;
        }
        for declarator_type in unrolled_type[..unrolled_type.len() - 1].iter() {
            match declarator_type {
                DeclaratorType::Array { size, .. } => {
                    if let Some(size) = size {
                        write!(f, "[{}]", size)?;
                    } else {
                        write!(f, "[]")?;
                    }
                }
                DeclaratorType::Function(function_declarator) => {
                    write!(f, "({}", joined(function_declarator.params.iter(), ", "))?;
                    if function_declarator.varargs {
                        write!(f, ", ...")?;
                    }
                    write!(f, ")")?;
                }
                DeclaratorType::Pointer { .. } => {
                    write!(f, ")")?;
                }
                DeclaratorType::End => unreachable!(),
            }
        }

        Ok(())
    }
}

impl fmt::Display for DeclaratorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // declarator with no id
        self.pretty_print(None, f)
    }
}

pub type Stmt = Locatable<StmtType>;
pub type CompoundStatement = Vec<Stmt>;

#[derive(Clone, Debug, PartialEq)]
pub enum StmtType {
    Compound(CompoundStatement),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Do(Box<Stmt>, Expr),
    While(Expr, Box<Stmt>),
    // for(int i = 1, j = 2; i < 4; ++i) body
    // for(i = 1; ; ++i) body
    // for (;;) ;
    For {
        initializer: Box<Stmt>,
        condition: Option<Box<Expr>>,
        post_loop: Option<Box<Expr>>,
        body: Box<Stmt>,
    },
    Switch(Expr, Box<Stmt>),
    Label(InternedStr, Box<Stmt>),
    Case(Box<Expr>, Box<Stmt>),
    Default(Box<Stmt>),
    Expr(Expr),
    Goto(InternedStr),
    Continue,
    Break,
    Return(Option<Expr>),
    Decl(Declaration),
}

pub type Expr = Locatable<ExprType>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum EncodingKind {
    Normal,
    Utf8,
    Utf16,
    Utf32,
    Wide,
}


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntegerRadix {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FloatRadix {
    Decimal,
    Hexadecimal,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralData {
    Integer {
        value: InternedStr,
        suffix: InternedStr,
        radix: IntegerRadix,
    },
    Float {
        significand: InternedStr,
        fraction: InternedStr,
        exponent: InternedStr,
        suffix: InternedStr,
        radix: FloatRadix,
    },
    String(String, EncodingKind),
    Char(InternedStr, EncodingKind),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprType {
    // primary
    Id(InternedStr),
    Literal(LiteralData),

    // postfix
    FuncCall(Box<Expr>, Vec<Expr>),
    Member(Box<Expr>, InternedStr),
    DerefMember(Box<Expr>, InternedStr),
    // post increment/decrement
    PostIncrement(Box<Expr>, bool),
    // a[i]
    Index(Box<Expr>, Box<Expr>),

    // prefix
    PreIncrement(Box<Expr>, bool),
    Cast(TypeName, Box<Expr>),
    AlignofType(TypeName),
    AlignofExpr(Box<Expr>),
    SizeofType(TypeName),
    SizeofExpr(Box<Expr>),
    Deref(Box<Expr>),
    AddressOf(Box<Expr>),
    UnaryPlus(Box<Expr>),
    Negate(Box<Expr>),
    BitwiseNot(Box<Expr>),
    LogicalNot(Box<Expr>),

    // binary
    LogicalOr(Box<Expr>, Box<Expr>),
    BitwiseOr(Box<Expr>, Box<Expr>),
    LogicalAnd(Box<Expr>, Box<Expr>),
    BitwiseAnd(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    // bool: left or right
    Shift(Box<Expr>, Box<Expr>, bool),
    // Token: make >, <, <=, ... part of the same variant
    Compare(Box<Expr>, Box<Expr>, ComparisonToken),
    // Token: allow extended assignment
    Assign(Box<Expr>, Box<Expr>, AssignmentToken),

    // misfits
    // Ternary: if ? then : else
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Comma(Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.data {
            ExprType::Comma(left, right) => write!(f, "{}, {}", *left, *right),
            // ExprType::Literal(token) => write!(f, "{}", token),
            ExprType::Literal(token) => todo!(),
            ExprType::Id(symbol) => write!(f, "{}", symbol),
            ExprType::Add(left, right) => write!(f, "({}) + ({})", left, right),
            ExprType::Sub(left, right) => write!(f, "({}) - ({})", left, right),
            ExprType::Mul(left, right) => write!(f, "({}) * ({})", left, right),
            ExprType::Div(left, right) => write!(f, "({}) / ({})", left, right),
            ExprType::Mod(left, right) => write!(f, "({}) % ({})", left, right),
            ExprType::Xor(left, right) => write!(f, "({}) ^ ({})", left, right),
            ExprType::BitwiseOr(left, right) => write!(f, "({}) | ({})", left, right),
            ExprType::BitwiseAnd(left, right) => write!(f, "({}) & ({})", left, right),
            ExprType::BitwiseNot(expr) => write!(f, "(~{})", expr),
            ExprType::Deref(expr) => write!(f, "*({})", expr),
            ExprType::Negate(expr) => write!(f, "-({})", expr),
            ExprType::UnaryPlus(expr) => write!(f, "+({})", expr),
            ExprType::LogicalNot(expr) => write!(f, "!({})", expr),
            ExprType::LogicalOr(left, right) => write!(f, "({}) || ({})", left, right),
            ExprType::LogicalAnd(left, right) => write!(f, "({}) && ({})", left, right),
            ExprType::Shift(val, by, left) => {
                write!(f, "({}) {} ({})", val, if *left { "<<" } else { ">>" }, by)
            }
            ExprType::Compare(left, right, token) => write!(f, "({}) {} ({})", left, token, right),
            ExprType::Assign(left, right, token) => write!(f, "({}) {} ({})", left, token, right),
            ExprType::Ternary(cond, left, right) => {
                write!(f, "({}) ? ({}) : ({})", cond, left, right)
            }
            ExprType::FuncCall(left, params) => write!(f, "({})({})", left, joined(params, ", ")),
            ExprType::Cast(ctype, expr) => write!(f, "({})({})", ctype, expr),
            ExprType::Member(compound, id) => write!(f, "({}).{}", compound, id),
            ExprType::DerefMember(compound, id) => write!(f, "({})->{}", compound, id),
            ExprType::PreIncrement(expr, inc) => {
                write!(f, "{}({})", if *inc { "++" } else { "--" }, expr)
            }
            ExprType::PostIncrement(expr, inc) => {
                write!(f, "({}){}", expr, if *inc { "++" } else { "--" })
            }
            ExprType::Index(array, index) => write!(f, "({})[{}]", array, index),
            // intrinsics
            ExprType::AddressOf(expr) => write!(f, "&({})", expr),
            ExprType::SizeofExpr(expr) => write!(f, "sizeof({})", expr),
            ExprType::SizeofType(ty) => write!(f, "sizeof({})", ty),
            ExprType::AlignofExpr(expr) => write!(f, "alignof({})", expr),
            ExprType::AlignofType(ty) => write!(f, "alignof({})", ty),
        }
    }
}

impl Default for StmtType {
    fn default() -> Self {
        StmtType::Compound(Vec::new())
    }
}
