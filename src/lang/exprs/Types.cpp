#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "../Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<TypeIdentExpr> TypeIdentExpr::pull(Stream& stream) {
    Rollback rb(stream);
    // `void` is parsed as a void literal
    if (auto lit = Token::draw<Lit>(stream)) {
        if (std::holds_alternative<VoidLit>(lit.value())) {
            return rb.commit<TypeIdentExpr>(stream.make<IdentExpr>(IdentPath("void")));
        }
        else {
            return rb.error("Expected type name, got literal");
        }
    }
    GEODE_UNWRAP_INTO(auto value, IdentExpr::pull(stream));
    return rb.commit<TypeIdentExpr>(value);
}

Type TypeIdentExpr::typecheck(UnitParser& state) const {
    auto ty = state.getType(expr->path);
    if (!ty) {
        state.error(expr->range, "Unknown type \"{}\"", expr->path);
        return Primitive::Unk;
    }
    return *ty;
}

std::string TypeIdentExpr::debug(size_t indent) const {
    return DebugPrint("TypeIdentExpr", indent)
        .member("expr", expr);
}

ExprResult<AliasExpr> AliasExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull(Keyword::Using, stream));
    GEODE_UNWRAP_INTO(auto ident, IdentExpr::pull(stream));
    GEODE_UNWRAP(Token::pull(Op::Seq, stream));
    GEODE_UNWRAP_INTO(auto type, TypeExpr::pull(stream));
    return rb.commit<AliasExpr>(ident, type);
}

Type AliasExpr::typecheck(UnitParser& state) const {
    state.verifyCanPush(alias);
    AliasType ty;
    ty.alias = alias->path;
    ty.type = type->typecheck(state);
    auto ret = Type(ty, shared_from_this());
    state.push(ret);
    return ret;
}

std::string AliasExpr::debug(size_t indent) const {
    return DebugPrint("AliasExpr", indent)
        .member("alias", alias)
        .member("type", type);
}
