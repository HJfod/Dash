#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "../Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<VarDeclExpr> VarDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull(Keyword::Let, stream));
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));

    Option<Rc<TypeExpr>> type;
    if (Token::pull(':', stream)) {
        GEODE_UNWRAP_INTO(type, TypeExpr::pull(stream));
    }
    rb.clearMessages();

    Option<Rc<Expr>> value;
    if (Token::pull(Op::Seq, stream)) {
        GEODE_UNWRAP_INTO(value, Expr::pull(stream));
    }
    rb.clearMessages();

    return rb.commit<VarDeclExpr>(ident, type, value);
}

Type VarDeclExpr::typecheck(UnitParser& state) const {
    Type varty;
    if (!type && !value) {
        state.error(range, "Variable needs an explicit type or value");
        varty = Type(UnkType());
    }
    if (type) {
        auto ty = type.value()->typecheck(state);
        if (value) {
            auto vt = value.value()->typecheck(state);
            if (ty.convertible(vt)) {
                // todo: hint what the types are
                state.error(range, "Value does not match explicit type");
            }
        }
        varty = ty;
    }
    if (value) {
        varty = value.value()->typecheck(state);
    }
    if (state.getVar(ident, true)) {
        state.error(range, "Variable \"{}\" already exists in this scope", ident);
    }
    else {
        state.pushVar(Var {
            .name = ident,
            .type = varty,
        });
    }
    return varty;
}

std::string VarDeclExpr::debug(size_t indent) const {
    return DebugPrint("VarDeclExpr", indent)
        .member("ident", ident)
        .member("type", type)
        .member("value", value);
}

ParseResult<> FunDeclExpr::pullParams(Vec<Param>& target, Stream& stream, bool requireTypes) {
    while (true) {
        auto start = stream.location();
        stream.debugTick();
        Param param;
        GEODE_UNWRAP_INTO(param.name, Token::pull<Ident>(stream));
        if (requireTypes) {
            GEODE_UNWRAP(Token::pull(':', stream));
            GEODE_UNWRAP_INTO(param.type, TypeExpr::pull(stream));
        }
        else if (Token::draw(':', stream)) {
            GEODE_UNWRAP_INTO(param.type, TypeExpr::pull(stream));
        }
        if (Token::draw(Op::Eq, stream)) {
            GEODE_UNWRAP_INTO(param.value, Expr::pull(stream));
        }
        param.range = Range(start, stream.location());
        target.push_back(param);
        GEODE_UNWRAP_INTO(auto brk, Token::pullSeparator(',', ')', stream));
        if (brk) {
            break;
        }
    }
    return Ok();
}

ExprResult<FunDeclExpr> FunDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull(Keyword::Function, stream));
    auto name = Token::draw<Ident>(stream);
    GEODE_UNWRAP(Token::pull('(', stream));
    Vec<Param> params;
    GEODE_UNWRAP(FunDeclExpr::pullParams(params, stream, true));
    GEODE_UNWRAP(Token::pull(')', stream));
    Option<Rc<TypeExpr>> ret;
    if (Token::draw(Op::Arrow, stream)) {
        GEODE_UNWRAP_INTO(ret, TypeExpr::pull(stream));
    }
    GEODE_UNWRAP(Token::pull('{', stream));
    GEODE_UNWRAP_INTO(auto body, ListExpr::pull(stream));
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<FunDeclExpr>(name, params, body, ret);
}

ExprResult<FunDeclExpr> FunDeclExpr::pullArrow(Stream& stream) {
    Rollback rb(stream);
    Vec<Param> params;
    if (!Token::draw(Keyword::Get, stream)) {
        if (Token::draw(Keyword::Set, stream)) {
            GEODE_UNWRAP(Token::pull('(', stream));
            GEODE_UNWRAP(FunDeclExpr::pullParams(params, stream, false));
            GEODE_UNWRAP(Token::pull(')', stream));
        }
        else if (Token::draw('(', stream)) {
            GEODE_UNWRAP(FunDeclExpr::pullParams(params, stream, false));
            GEODE_UNWRAP(Token::pull(')', stream));
        }
        else {
            Param param;
            GEODE_UNWRAP_INTO(param.name, Token::pull<Ident>(stream));
            params.push_back(param);
        }
    }
    GEODE_UNWRAP(Token::pull(Op::Farrow, stream));
    Rc<Expr> body;
    GEODE_UNWRAP_INTO(body, Expr::pull(stream));
    return rb.commit<FunDeclExpr>(None, params, body, None);
}

Type FunDeclExpr::typecheck(UnitParser& state) const {
    state.pushScope(true);
    for (auto& param : params) {
        Type pty = Type(UnkType());
        if (param.type) {
            pty = param.type.value()->typecheck(state);
        }
        if (param.value) {
            auto vty = param.value.value()->typecheck(state);
            if (!param.type) {
                pty = vty;
            }
            else if (pty.convertible(vty)) {
                state.error(param.range, "Default value type does not match parameter type");
            }
        }
        state.pushVar(Var {
            .name = param.name,
            .type = pty,
        });
    }
    auto ret = body->typecheck(state);
    state.popScope();
    if (retType) {
        if (!retType.value()->typecheck(state).convertible(ret)) {
            state.error(body->range, "Function body does not match return type");
        }
    }
    return ret;
}

std::string FunDeclExpr::debug(size_t indent) const {
    return DebugPrint("FunDeclExpr", indent)
        .member("name", name)
        .member("params", params)
        .member("body", body);
}

ExprResult<MemberDeclExpr> MemberDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    auto required = Token::draw(Keyword::Required, stream).has_value();
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    GEODE_UNWRAP(Token::pull(':', stream));
    GEODE_UNWRAP_INTO(auto type, TypeExpr::pull(stream));
    Vec<Ident> dependencies;
    Option<Rc<FunDeclExpr>> getter;
    Option<Rc<FunDeclExpr>> setter;
    Option<Rc<Expr>> defaultValue;
    if (Token::draw('{', stream)) {
        while (true) {
            stream.debugTick();
            if (Token::draw(Keyword::Depends, stream)) {
                GEODE_UNWRAP_INTO(auto dep, Token::pull<Ident>(stream));
                dependencies.push_back(dep);
                GEODE_UNWRAP(Token::pullSemicolons(stream));
                continue;
            }
            if (Token::peek(Keyword::Get, stream)) {
                GEODE_UNWRAP_INTO(getter, FunDeclExpr::pullArrow(stream));
                GEODE_UNWRAP(Token::pullSemicolons(stream));
                continue;
            }
            if (Token::draw(Keyword::Set, stream)) {
                GEODE_UNWRAP_INTO(setter, FunDeclExpr::pullArrow(stream));
                GEODE_UNWRAP(Token::pullSemicolons(stream));
                continue;
            }
            break;
        }
        if (getter.has_value() ^ setter.has_value()) {
            return rb.errorNextToken("A getter and setter need to both be defined");
        }
        GEODE_UNWRAP(Token::pull('}', stream));
    }
    else if (Token::draw(Op::Bind, stream)) {
        // multi-bind
        if (Token::draw('{', stream)) {
            Vec<Rc<PropExpr>> props;
            Vec<Rc<Expr>> exprs;
            Map<Ident, Ident> bindedDeps;
            while (true) {
                stream.debugTick();
                GEODE_UNWRAP_INTO(auto dep, Token::pull<Ident>(stream));
                if (Token::draw(Op::Bind, stream)) {
                    GEODE_UNWRAP_INTO(auto bindTo, Token::pull<Ident>(stream));
                    bindedDeps[dep] = bindTo;
                    dependencies.push_back(bindTo);
                }
                else {
                    bindedDeps[dep] = dep;
                    dependencies.push_back(dep);
                }
                GEODE_UNWRAP_INTO(auto brk, Token::pullSeparator(',', '}', stream));
                if (brk) {
                    break;
                }
            }
            GEODE_UNWRAP(Token::pull('}', stream));
            for (auto& [bind, dep] : bindedDeps) {
                props.push_back(stream.make<PropExpr>(
                    bind, stream.make<MemberExpr>(stream.make<IdentExpr>("this"), dep)
                ));
                exprs.push_back(stream.make<BinOpExpr>(
                    stream.make<MemberExpr>(stream.make<IdentExpr>("this"), dep),
                    stream.make<MemberExpr>(stream.make<IdentExpr>("value"), bind),
                    Op::Seq
                ));
            }
            getter = stream.make<FunDeclExpr>(
                None, Vec<Param>(),
                stream.make<NodeExpr>(None, props, Vec<Rc<NodeExpr>>()),
                stream.make<TypeIdentExpr>(ident)
            );
            setter = stream.make<FunDeclExpr>(
                None, Vec<Param> { Param {
                    .name = "value",
                    .type = stream.make<TypeIdentExpr>(ident)
                } },
                stream.make<ListExpr>(exprs),
                None
            );
        }
        else {
            GEODE_UNWRAP_INTO(auto dep, Token::pull<Ident>(stream));
            dependencies.push_back(dep);
            getter = stream.make<FunDeclExpr>(
                None, Vec<Param>(),
                stream.make<MemberExpr>(stream.make<IdentExpr>("this"), dep),
                stream.make<TypeIdentExpr>(ident)
            );
            setter = stream.make<FunDeclExpr>(
                None, Vec<Param> { Param {
                    .name = "value",
                    .type = stream.make<TypeIdentExpr>(ident)
                } },
                stream.make<BinOpExpr>(
                    stream.make<MemberExpr>(stream.make<IdentExpr>("this"), dep),
                    stream.make<IdentExpr>("value"),
                    Op::Seq
                ),
                None
            );
        }
    }
    if (Token::draw(Op::Eq, stream)) {
        GEODE_UNWRAP_INTO(defaultValue, Expr::pull(stream));
    }
    return rb.commit<MemberDeclExpr>(ident, type, defaultValue, dependencies, getter, setter, required);
}

Type MemberDeclExpr::typecheck(UnitParser& state) const {
    if (defaultValue && (getter || setter)) {
        state.error(range, "Properties may not have both a setter / getter and default value");
    }
    auto ty = type->typecheck(state);
    if (defaultValue) {
        auto valty = defaultValue.value()->typecheck(state);
        if (ty.convertible(valty)) {
            // todo: hint types
            state.error(range, "Value and member type do not match");
        }
    }
    if (getter) {
        if (!getter.value()->typecheck(state).convertible(ty)) {
            state.error(getter.value()->range, "Getter return type does not match member type");
        }
    }
    if (setter) {
        // todo: verify setter parameter type
        setter.value()->typecheck(state);
    }
    return ty;
}

std::string MemberDeclExpr::debug(size_t indent) const {
    return DebugPrint("MemberDeclExpr", indent)
        .member("name", name)
        .member("type", type)
        .member("defaultValue", defaultValue)
        .member("dependencies", dependencies)
        .member("getter", getter)
        .member("setter", setter)
        .member("required", required);
}

ExprResult<NodeDeclExpr> NodeDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    auto isExtern = Token::draw(Keyword::Extern, stream).has_value();
    auto isStruct = Token::draw(Keyword::Struct, stream).has_value();
    if (!isStruct) {
        GEODE_UNWRAP(Token::pull(Keyword::Decl, stream));
    }
    auto ident = Token::draw<Ident>(stream);
    Option<Rc<IdentExpr>> extends;
    if (Token::draw(Keyword::Extends, stream)) {
        GEODE_UNWRAP_INTO(extends, IdentExpr::pull(stream));
    }
    GEODE_UNWRAP(Token::pull('{', stream));
    Vec<Rc<MemberDeclExpr>> members;
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto mem, MemberDeclExpr::pull(stream));
        members.push_back(mem);
        GEODE_UNWRAP(Token::pullSemicolons(stream));
        if (Token::peek('}', stream)) {
            break;
        }
    }
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<NodeDeclExpr>(ident, members, extends, isStruct, isExtern);
}

Type NodeDeclExpr::typecheck(UnitParser& state) const {
    if (ident) {
        if (state.getType(ident.value(), true)) {
            state.error(range, "Type \"{}\" has already been defined in this scope", ident.value());
            return Type(UnkType());
        }
    }
    if (isStruct) {
        StructType sty;
        sty.name = ident;
        for (auto& mem : members) {
            auto mty = mem->typecheck(state);
            sty.members[mem->name] = PropType {
                .type = mty,
                .dependencies = mem->dependencies,
                .required = mem->defaultValue || mem->getter,
            };
        }
        if (ident) {
            state.pushType(Type(sty));
        }
        return Type(sty);
    }
    else {
        if (!ident) {
            state.error(range, "Anonymous nodes are not allowed");
            return Type(UnkType());
        }
        NodeType sty;
        sty.name = ident.value();
        for (auto& mem : members) {
            auto mty = mem->typecheck(state);
            sty.props[mem->name] = PropType {
                .type = mty,
                .dependencies = mem->dependencies,
                .required = false,
            };
        }
        state.pushType(Type(sty));
        return Type(sty);
    }
}

std::string NodeDeclExpr::debug(size_t indent) const {
    return DebugPrint("NodeDeclExpr", indent)
        .member("ident", ident)
        .member("members", members)
        .member("extends", extends)
        .member("isStruct", isStruct)
        .member("isExtern", isExtern);
}
