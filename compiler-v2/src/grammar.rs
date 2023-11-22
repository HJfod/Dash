
enum Type {
    Rule(String),
    Optional(String),
    List(String),
}

struct Member {
    name: String,
    ty: Type,
}

struct Layout {
    members: Vec<Member>,
}

enum ItemRef {
    Keyword(String),
    Rule(String),
}

enum GrammarLine {
    Match {
        what: ItemRef,
        into: String,
    },
    Peek {
        what: ItemRef,
    },
    If {
        cond: Box<GrammarLine>,
        truthy: Grammar,
        falsy: Grammar,
    },
}

type Grammar = Vec<GrammarLine>;

struct Rule {
    layout: Layout,
    grammar: Grammar,
}

struct Runner {}
