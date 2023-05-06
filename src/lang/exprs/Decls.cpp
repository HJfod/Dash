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
            if (ty != vt) {
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

ExprResult<MemberDeclExpr> MemberDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    GEODE_UNWRAP(Token::pull(':', stream));
    GEODE_UNWRAP_INTO(auto type, TypeExpr::pull(stream));
    return rb.commit<MemberDeclExpr>(ident, type);
}

Type MemberDeclExpr::typecheck(UnitParser& state) const {
    return type->typecheck(state);
}

std::string MemberDeclExpr::debug(size_t indent) const {
    return DebugPrint("MemberDeclExpr", indent)
        .member("name", name)
        .member("type", type);
}

ExprResult<StructDeclExpr> StructDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull(Keyword::Struct, stream));
    auto ident = Token::pull<Ident>(stream).ok();
    rb.clearMessages();
    GEODE_UNWRAP(Token::pull('{', stream));
    Vec<Rc<MemberDeclExpr>> members;
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto mem, MemberDeclExpr::pull(stream));
        members.push_back(mem);
        GEODE_UNWRAP_INTO(auto brk, Token::pullSeparator(',', '}', stream));
        if (brk) {
            break;
        }
    }
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<StructDeclExpr>(ident, members);
}

Type StructDeclExpr::typecheck(UnitParser& state) const {
    if (ident) {
        if (state.getType(ident.value(), true)) {
            state.error(range, "Struct type \"{}\" has already been defined in this scope", ident.value());
            return Type(UnkType());
        }
    }
    StructType sty;
    sty.name = ident;
    for (auto& mem : members) {
        auto mty = mem->typecheck(state);
        sty.members[mem->name] = PropType {
            .type = mty,
            .defaultValue = None,
            .binding = None
        };
    }
    if (ident) {
        state.pushType(Type(sty));
    }
    return Type(sty);
}

std::string StructDeclExpr::debug(size_t indent) const {
    return DebugPrint("StructDeclExpr", indent)
        .member("ident", ident)
        .member("members", members);
}
