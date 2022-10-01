use crate::data::{AssignmentToken, ComparisonToken, LiteralValue};
use crate::location::Locatable;
use crate::InternedStr;

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

#[derive(Clone, Debug, PartialEq)]
pub struct StructSpecifier {
    pub name: Option<InternedStr>,
    /// Some([]): `struct s {}`
    /// None: `struct s;`
    pub members: Option<Vec<StructDeclarationList>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclarationList {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarators: Vec<StructDeclarator>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclarator {
    /// optional since this could be only padding bits
    pub decl: Option<Declarator>,
    pub bitfield: Option<Expr>,
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

#[derive(Clone, Debug, PartialEq)]
pub enum ExprType {
    // primary
    Id(InternedStr),
    Literal(LiteralValue),

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

impl Default for StmtType {
    fn default() -> Self {
        StmtType::Compound(Vec::new())
    }
}

