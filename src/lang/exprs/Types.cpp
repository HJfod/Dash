#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "../Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<TypeIdentExpr> TypeIdentExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto value, Token::pull<Ident>(stream));
    return rb.commit<TypeIdentExpr>(value);
}

Type TypeIdentExpr::typecheck(UnitParser& state) const {
    auto ty = state.getType(ident);
    if (!ty) {
        state.error(range, "Unknown type \"{}\"", ident);
        return Type(UnkType());
    }
    return *ty;
}

std::string TypeIdentExpr::debug(size_t indent) const {
    return DebugPrint("TypeIdentExpr", indent)
        .member("ident", ident);
}

ExprResult<TypeExpr> TypeExpr::pull(Stream& stream) {
    GEODE_UNWRAP_INTO(auto ty, TypeIdentExpr::pull(stream));
    return Ok(ty);
}
