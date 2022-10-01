use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

use crate::data::{ComparisonToken, LiteralValue, StorageClass};
use crate::location::{Locatable, Location};

use crate::preprocess::Keyword;
use crate::InternedStr;

use super::types::TypeKind;

pub type Stmt = Locatable<StmtType>;

#[derive(Clone, Debug, PartialEq)]
pub enum StmtType {
    Compound(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Do(Box<Stmt>, Expr),
    While(Expr, Box<Stmt>),
    // for(int i = 1, j = 2; i < 4; ++i) body
    // for(i = 1; ; ++i) body
    // for (;;) ;
    For(Box<Stmt>, Option<Box<Expr>>, Option<Box<Expr>>, Box<Stmt>),
    Switch(Expr, Box<Stmt>),
    Label(InternedStr, Box<Stmt>),
    Case(u64, Box<Stmt>),
    Default(Box<Stmt>),
    Expr(Expr),
    Goto(InternedStr),
    Continue,
    Break,
    Return(Option<Expr>),
    Decl(Vec<Locatable<Declaration>>),
}

impl Default for StmtType {
    fn default() -> Self {
        StmtType::Compound(Vec::new())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub symbol: Symbol,
    pub init: Option<Initializer>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Initializer {
    Scalar(Box<Expr>),                 // int i = 5;
    InitializerList(Vec<Initializer>), // int a[] = { 1, 2, 3 };
    FunctionBody(Vec<Stmt>),           // int f() { return 0; }
}

/// Holds the metadata for an expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    /// expr: holds the actual expression
    pub expr: ExprType,

    /// ctype: holds the type of the expression
    pub ctype: TypeKind,

    /// lval: whether an expression can be assigned to
    ///
    /// for example, variables, array elements, and pointer dereferences are lvals,
    /// but literals, functions, and addresses are not
    pub lval: bool,

    /// location: the best approximation of where the expression is
    ///
    /// usually points to the location of the operation symbol, or the literal if no
    /// operations is being performed
    /// implicit operations should point to the child expression
    pub location: Location,
}

/// An identifier used to look up the metadata for a variable.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Symbol(usize);

thread_local!(
    /// The global storage for all metadata.
    ///
    /// The type is read like so:
    /// RefCell: A container with interior mutability, used because `LocalKey`
    /// returns an immutable reference.
    /// MetadataStore: metadata for all variables seen so far
    static SYMBOL_TABLE: RefCell<SymbolTable> = Default::default()
);

#[derive(Default)]
struct SymbolTable(Vec<Rc<Variable>>);

impl SymbolTable {
    fn insert(&mut self, m: Variable) -> Symbol {
        let i = self.0.len();
        self.0.push(Rc::new(m));
        Symbol(i)
    }
    /// Guaranteed not to panic since `MetadataRef` is always valid
    pub(crate) fn get(&self, i: Symbol) -> Rc<Variable> {
        self.0[i.0].clone()
    }
}

impl Symbol {
    pub fn get(self) -> Rc<Variable> {
        SYMBOL_TABLE.with(|store| store.borrow().get(self))
    }
}

impl Variable {
    pub fn insert(self) -> Symbol {
        SYMBOL_TABLE.with(|store| store.borrow_mut().insert(self))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprType {
    // primary expressions
    // This stores a reference to the metadata for the identifier,
    // which can be looked up using a `metadata_store`.
    Id(Symbol),
    Literal(LiteralValue),
    FuncCall(Box<Expr>, Vec<Expr>),
    Member(Box<Expr>, InternedStr),

    // unary expressions
    // post increment/decrement
    PostIncrement(Box<Expr>, bool),
    Cast(Box<Expr>),
    Sizeof(TypeKind),
    Deref(Box<Expr>),
    Negate(Box<Expr>),
    BitwiseNot(Box<Expr>),

    // binary expressions
    Binary(BinaryOp, Box<Expr>, Box<Expr>),

    // misfits
    // Ternary: if ? then : else
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Comma(Box<Expr>, Box<Expr>),
    // &expr in static context
    // requires cooperation with the linker
    StaticRef(Box<Expr>),
    // used to work around various bugs, see places this is constructed for details
    Noop(Box<Expr>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    // binary expressions
    LogicalOr,
    BitwiseOr,
    LogicalAnd,
    BitwiseAnd,
    Xor,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    // Token: make >, <, <=, ... part of the same variant
    Compare(ComparisonToken),
    Assign,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryOp::*;
        let s = match self {
            LogicalOr => "||",
            BitwiseOr => "|",
            LogicalAnd => "&&",
            BitwiseAnd => "&",
            Xor => "^",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Add => "+",
            Sub => "-",
            Shl => "<<",
            Shr => ">>",
            Compare(compare) => return write!(f, "{}", compare),
            Assign => "=",
        };
        write!(f, "{}", s)
    }
}

/* structs */
/// The metadata stored for variables and function parameters.
///
/// For abstract function parameters, e.g. `int f(int)`, the `id` will resolve to the empty string.
/// Furthermore, it is guaranteed to be equal to `InternedStr::default()`.
#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub ctype: TypeKind,
    pub storage_class: StorageClass,
    pub qualifiers: Qualifiers,
    pub id: InternedStr,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Qualifiers {
    pub volatile: bool,
    pub c_const: bool,
    pub func: FunctionQualifiers,
}

#[cfg_attr(test, derive(Arbitrary))]
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct FunctionQualifiers {
    pub inline: bool,
    pub no_return: bool,
}

#[derive(Debug)]
pub(crate) struct Scope<K: Hash + Eq, V>(Vec<HashMap<K, V>>);

impl Qualifiers {
    pub(crate) fn has_func_qualifiers(self) -> bool {
        self.func.inline || self.func.no_return
    }
    // TODO: this should just be a Default
    pub const NONE: Qualifiers = Qualifiers {
        c_const: false,
        volatile: false,
        func: FunctionQualifiers {
            inline: false,
            no_return: false,
        },
    };
}

impl Display for FunctionQualifiers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.inline, self.no_return) {
            (true, true) => write!(f, "{} {}", Keyword::Inline, Keyword::NoReturn),
            (true, false) => write!(f, "{}", Keyword::NoReturn),
            (false, true) => write!(f, "{}", Keyword::NoReturn),
            (false, false) => Ok(()),
        }
    }
}

impl<K: Hash + Eq, V> Scope<K, V> {
    #[inline]
    pub(crate) fn new() -> Self {
        Self(vec![HashMap::new()])
    }
    #[inline]
    pub(crate) fn enter(&mut self) {
        self.0.push(HashMap::<K, V>::new())
    }
    #[inline]
    pub(crate) fn exit(&mut self) {
        self.0.pop();
    }
    pub(crate) fn get(&self, name: &K) -> Option<&V> {
        self.iter()
            .filter_map(|(key, value)| if key == name { Some(value) } else { None })
            .next()
    }
    // returns whether the _immediate_ scope contains `name`
    #[inline]
    pub(crate) fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.0.last_mut().unwrap().insert(key, value)
    }
    #[inline]
    pub(crate) fn get_immediate(&self, name: &K) -> Option<&V> {
        self.0.last().unwrap().get(name)
    }
    #[inline]
    pub(crate) fn get_all_immediate(&mut self) -> &mut HashMap<K, V> {
        self.0.last_mut().unwrap()
    }
    pub(crate) fn is_global(&self) -> bool {
        self.0.len() == 1
    }
    pub(crate) fn _remove(&mut self, key: &K) -> Option<V> {
        debug_assert!(!self.0.is_empty());
        self.0.last_mut().unwrap().remove(key)
    }
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.0.iter().rev().flatten()
    }
}

impl<K: Eq + Hash, V> Default for Scope<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for Qualifiers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut basic_quals = match (self.c_const, self.volatile) {
            (true, true) => "const volatile",
            (true, false) => "const",
            (false, true) => "volatile",
            (false, false) => "",
        }
        .to_owned();
        let func_quals = match (self.func.inline, self.func.no_return) {
            (true, true) => "inline _Noreturn",
            (true, false) => "inline",
            (false, true) => "_Noreturn",
            (false, false) => "",
        };
        if basic_quals != "" && func_quals != "" {
            basic_quals.push(' ');
        }
        basic_quals.push_str(func_quals);
        write!(f, "{}", basic_quals)
    }
}

/*
impl PartialEq for Symbol {
    // don't require both symbols to be `init` to be equal
    fn eq(&self, other: &Self) -> bool {
        self.ctype == other.ctype
            && self.id == other.id
            && self.qualifiers == other.qualifiers
            && (self.storage_class == other.storage_class
                || !self.ctype.is_function()
                    && (self.storage_class == StorageClass::Auto
                        && other.storage_class == StorageClass::Extern
                        || self.storage_class == StorageClass::Extern
                            && other.storage_class == StorageClass::Auto))
    }
}

impl Eq for Symbol {}
*/
