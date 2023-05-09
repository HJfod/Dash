#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "../Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<VarDeclExpr> VarDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull(Keyword::Let, stream));
    GEODE_UNWRAP_INTO(auto ident, IdentExpr::pull(stream));

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
        varty = Type(UnkType(), nullptr);
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
    state.verifyCanPush(ident);
    state.pushVar(Var {
        .name = ident->path,
        .type = varty,
    });
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
    Name name = None;
    Vec<Param> params;
    Option<Rc<TypeExpr>> ret;
    // special operator syntax `fun <ident>: <type> <op> <ident>: <type>`
    if (Token::peek<Ident>(stream) && Token::peek(':', stream, 1)) {
        auto p1start = stream.location();
        GEODE_UNWRAP_INTO(auto ident1, Token::pull<Ident>(stream));
        GEODE_UNWRAP(Token::pull(':', stream));
        GEODE_UNWRAP_INTO(auto type1, TypeExpr::pull(stream));
        auto p1end = stream.location();
        params.push_back(Param {
            .name = ident1,
            .type = type1,
            .value = None,
            .range = Range(p1start, p1end),
        });
        if (Token::draw(Keyword::As, stream)) {
            name = AsName();
            GEODE_UNWRAP_INTO(ret, TypeExpr::pull(stream));
        }
        else {
            GEODE_UNWRAP_INTO(name, Token::pull<Op>(stream));
            auto p2start = stream.location();
            GEODE_UNWRAP_INTO(auto ident2, Token::pull<Ident>(stream));
            GEODE_UNWRAP(Token::pull(':', stream));
            GEODE_UNWRAP_INTO(auto type2, TypeExpr::pull(stream));
            auto p2end = stream.location();
            params.push_back(Param {
                .name = ident2,
                .type = type2,
                .value = None,
                .range = Range(p2start, p2end),
            });
            if (Token::draw(Op::Arrow, stream)) {
                GEODE_UNWRAP_INTO(ret, TypeExpr::pull(stream));
            }
        }
    }
    else {
        if (auto ident = IdentExpr::pull(stream)) {
            name = ident.unwrap();
        }
        else if (auto op = Token::pull<Op>(stream)) {
            name = op.unwrap();
        }
        rb.clearMessages();
        GEODE_UNWRAP(Token::pull('(', stream));
        GEODE_UNWRAP(FunDeclExpr::pullParams(params, stream, true));
        GEODE_UNWRAP(Token::pull(')', stream));
        if (Token::draw(Op::Arrow, stream)) {
            GEODE_UNWRAP_INTO(ret, TypeExpr::pull(stream));
        }
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
    FunType fun;
    if (std::holds_alternative<Rc<IdentExpr>>(name)) {
        fun.name = std::get<Rc<IdentExpr>>(name)->path;
    }
    fun.isExtern = isExtern;

    if (std::holds_alternative<Rc<IdentExpr>>(name)) {
        state.verifyCanPush(std::get<Rc<IdentExpr>>(name));
    }

    state.pushScope(true);
    for (auto& param : params) {
        Type pty = Type(UnkType(), nullptr);
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
            .name = IdentPath(param.name),
            .type = pty,
            .decl = shared_from_this(),
        });
        fun.params.push_back(ParamType {
            .name = param.name,
            .type = pty,
        });
    }
    if (retType) {
        fun.retType = retType.value()->typecheck(state);
    }

    auto ret = Type(fun, shared_from_this());

    // allow recursion by pushing fun type before body check
    state.scope(1).push(ret);

    if (!body->typecheck(state).convertible(fun.retType)) {
        state.error(body->range, "Function body does not match return type");
    }
    state.popScope();

    return ret;
}

std::string FunDeclExpr::debug(size_t indent) const {
    return DebugPrint("FunDeclExpr", indent)
        .member("name", std::visit(makeVisitor {
            [](std::nullopt_t const&) {
                return std::string("none");
            },
            [](Rc<IdentExpr> const& expr) {
                return expr->path.toString();
            },
            [](Op const& op) {
                return tokenToString(op, true);
            },
            [](AsName const&) {
                return std::string("as");
            },
        }, name))
        .member("params", params)
        .member("body", body)
        .member("retType", retType)
        .member("isExtern", isExtern);
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
                    bind, stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("this")), dep)
                ));
                exprs.push_back(stream.make<BinOpExpr>(
                    stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("this")), dep),
                    stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("value")), bind),
                    Op::Seq
                ));
            }
            getter = stream.make<FunDeclExpr>(
                None, Vec<Param>(),
                stream.make<NodeExpr>(None, props, Vec<Rc<NodeExpr>>()),
                stream.make<TypeIdentExpr>(stream.make<IdentExpr>(IdentPath(ident)))
            );
            setter = stream.make<FunDeclExpr>(
                None, Vec<Param> { Param {
                    .name = "value",
                    .type = stream.make<TypeIdentExpr>(stream.make<IdentExpr>(IdentPath(ident)))
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
                stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("this")), dep),
                stream.make<TypeIdentExpr>(stream.make<IdentExpr>(IdentPath(ident)))
            );
            setter = stream.make<FunDeclExpr>(
                None, Vec<Param> { Param {
                    .name = "value",
                    .type = stream.make<TypeIdentExpr>(stream.make<IdentExpr>(IdentPath(ident)))
                } },
                stream.make<BinOpExpr>(
                    stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("this")), dep),
                    stream.make<IdentExpr>(IdentPath("value")),
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

ExprResult<NodeDeclExpr> NodeDeclExpr::pull(Stream& stream, bool implicitStruct) {
    Rollback rb(stream);
    bool isExtern = false;
    bool isStruct;
    Option<Rc<IdentExpr>> ident;
    if (implicitStruct) {
        isStruct = true;
        GEODE_UNWRAP_INTO(ident, IdentExpr::pull(stream));
    }
    else {
        isExtern = Token::draw(Keyword::Extern, stream).has_value();
        isStruct = Token::draw(Keyword::Struct, stream).has_value();
        if (!isStruct) {
            GEODE_UNWRAP(Token::pull(Keyword::Decl, stream));
        }
        ident = IdentExpr::pull(stream).ok();
        rb.clearMessages();
    }
    Option<Rc<IdentExpr>> extends;
    if (Token::draw(Keyword::Extends, stream)) {
        GEODE_UNWRAP_INTO(extends, IdentExpr::pull(stream));
    }
    GEODE_UNWRAP(Token::pull('{', stream));
    Vec<Rc<MemberDeclExpr>> members;
    while (!Token::peek('}', stream)) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto mem, MemberDeclExpr::pull(stream));
        members.push_back(mem);
        GEODE_UNWRAP(Token::pullSemicolons(stream));
    }
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<NodeDeclExpr>(ident, members, extends, isStruct, isExtern);
}

Type NodeDeclExpr::typecheck(UnitParser& state) const {
    if (ident) {
        state.verifyCanPush(ident.value());
    }
    if (isStruct) {
        StructType sty;
        if (ident) {
            sty.name = ident.value()->path;
        }
        sty.isExtern = isExtern;
        for (auto& mem : members) {
            auto mty = mem->typecheck(state);
            sty.members[mem->name] = PropType {
                .type = mty,
                .dependencies = mem->dependencies,
                .required = mem->defaultValue || mem->getter,
            };
        }
        auto ret = Type(sty, shared_from_this());
        if (ident) {
            state.pushType(ret);
        }
        return ret;
    }
    else {
        if (!ident) {
            state.error(range, "Anonymous nodes are not allowed");
            return Type(UnkType(), nullptr);
        }
        NodeType sty;
        sty.name = ident.value()->path;
        for (auto& mem : members) {
            auto mty = mem->typecheck(state);
            sty.props[mem->name] = PropType {
                .type = mty,
                .dependencies = mem->dependencies,
                .required = false,
            };
        }
        auto ret = Type(sty, shared_from_this());
        state.pushType(ret);
        return ret;
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

ExprResult<EnumDeclExpr> EnumDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    auto isExtern = Token::draw(Keyword::Extern, stream).has_value();
    GEODE_UNWRAP(Token::pull(Keyword::Enum, stream));
    auto ident = IdentExpr::pull(stream).ok();
    rb.clearMessages();
    GEODE_UNWRAP(Token::pull('{', stream));
    Vec<Rc<NodeDeclExpr>> variants;
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto var, NodeDeclExpr::pull(stream, true));
        variants.push_back(var);
        GEODE_UNWRAP(Token::pullSemicolons(stream));
        if (Token::peek('}', stream)) {
            break;
        }
    }
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<EnumDeclExpr>(ident, variants, isExtern);
}

Type EnumDeclExpr::typecheck(UnitParser& state) const {
    if (ident) {
        state.verifyCanPush(ident.value());
    }
    EnumType ty;
    if (ident) {
        ty.name = ident.value()->path;
    }
    ty.isExtern = isExtern;
    for (auto& var : variants) {
        if (!var->ident) {
            throw std::runtime_error("Anonymous enum variant encountered");
        }
        auto name = var->ident.value();
        if (!name->path.isSingle()) {
            state.error(name->range, "Variant name may not be a full path");
        }
        else {
            if (ty.variants.contains(name->path.name)) {
                state.error(var->range, "Variant \"{}\" already defined", name->path);
            }
            else {
                ty.variants.insert({ name->path.name, var->typecheck(state) });
            }
        }
    }
    auto ret = Type(ty, shared_from_this());
    if (ident) {
        state.pushType(ret);
    }
    return ret;
}

std::string EnumDeclExpr::debug(size_t indent) const {
    return DebugPrint("EnumDeclExpr", indent)
        .member("ident", ident)
        .member("variants", variants)
        .member("isExtern", isExtern);
}
