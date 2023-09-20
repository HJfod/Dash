
public enum Type {
    Never,
    Void,
    Bool,
    Int,
    Float,
    String,
    Function {
        name: string,
        params: Type[],
        returnType: Type,
    },
    Option {
        type: Type,
    }
}

public struct FieldDecl {
    name: string,
    type: Type,
}

public struct StructDecl {
    name: string,
    fields: FieldDecl[],
}

public struct ParamDecl {
    name: string,
    type: Type,
    value: Expr,
}

public struct FunctionDecl {
    name: string?,
    params: ParamDecl[],
}

public enum Expr {
    FunctionDecl {
        ref: FunctionDecl,
    },
    StructDecl {
        ref: StructDecl,
    },
}
