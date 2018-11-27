extern crate ress;
extern crate ressa;
use ress::{Comment, CommentKind, Item, Token};
use ressa::{CommentHandler, Parser};

fn main() {}

struct JSDocHandler {
    pub docs: Vec<JSDoc>,
}

impl CommentHandler for JSDocHandler {
    fn handle_comment(&mut self, comment: Item) {}
}

struct JSDoc {
    pub preamble: String,
    pub properties: Vec<Prop>,
}

enum Prop {
    Abstract,
    Access(Access),
    Alias(String),
    Async,
    Augments,
    Author(Author),
    Borrows(Borrow),
    Callback(String),
    Class(Detail),
    ClassDesc(String),
    Constant(Detail),
    Constructs(String),
    Copyright(String),
    Default(String),
    Deprecated(String),
    Description(String),
    Enum(String),
    Event(String, Option<String>, String),
    Example(String),
    Exports(String),
    External(String),
    File(String),
    Fires(String, Option<String>, String),
    Function(String),
    Generator,
    Global,
    HideConstructor,
    Ignore,
    Implements(String),
    InheritDoc,
    Inner,
    Instance,
    Interface(Option<String>),
    Kind(Symbol),
    Lends(String),
    License(String),
    Listens(String),
    Member(Detail),
    MemberOf(bool, String),
    Mixes(String),
    Mixin(Option<String>),
    Module(Option<String>, String),
    Name(String),
    Namespace(Detail),
    Override,
    Package(Option<String>),
    Param(Detail),
    Private(Option<String>),
    Property,
    Protected(Option<String>),
    Public(Option<String>),
    Readonly,
    Requires(String),
    Returns(Detail),
    See(String),
    Since(String),
    Static,
    Summary(String),
    This(String),
    Throws(Detail),
    Todo(String),
    Tutorial,
    Type(String),
    TypeDef(String, String),
    Variation(String),
    Version(String),
    Yields(Detail),
}

enum Access {
    Package,
    Public,
    Private,
    Protected,
}

struct Author {
    pub name: String,
    pub email: Option<String>,
}

struct Borrow {
    from: String,
    to: String,
}

struct Detail {
    kind: Option<String>,
    name: Option<String>,
    desc: Option<String>,
}

enum Symbol {
    Class,
    Constant,
    Event,
    External,
    File,
    Function,
    Member,
    Mixin,
    Module,
    Namespace,
    Typedef,
}
