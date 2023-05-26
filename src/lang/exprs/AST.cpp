#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "../Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<AttrExpr> AttrExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull('@', stream));
    GEODE_UNWRAP_INTO(auto ident, IdentExpr::pull(stream));
    Option<Rc<Expr>> value;
    if (Token::draw('(', stream)) {
        GEODE_UNWRAP_INTO(value, Expr::pull(stream));
        GEODE_UNWRAP(Token::pull(')', stream));
    }
    return rb.commit<AttrExpr>(ident, value);
}

Type AttrExpr::typecheck(UnitParser& state) const {
    if (value) {
        value.value()->typecheck(state);
    }
    return Primitive::Void;
}

std::string AttrExpr::debug(size_t indent) const {
    return DebugPrint("AttrExpr", indent)
        .member("attribute", attribute)
        .member("value", value);
}

ExprResult<ExportExpr> ExportExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull(Keyword::Export, stream));
    GEODE_UNWRAP_INTO(auto expr, Expr::pull(stream));
    return rb.commit<ExportExpr>(expr);
}

Type ExportExpr::typecheck(UnitParser& state) const {
    if (auto ent = expr->typecheckEntity(state)) {
        state.getParsedSrc()->addExported(state, range, ent.value());
        return ent.value().getType().value_or(Primitive::Unk);
    }
    else {
        state.error(range, "Only declarations are exportable");
    }
    if (!state.isRootScope()) {
        state.error(range, "Export statements may only appear at top-level");
    }
    return Primitive::Unk;
}

std::string ExportExpr::debug(size_t indent) const {
    return DebugPrint("ExportExpr", indent)
        .member("expr", expr);
}

ExprResult<ImportExpr> ImportExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull(Keyword::Import, stream));
    Rollback litrb(stream);
    GEODE_UNWRAP_INTO(auto lit, Token::pull<Lit>(stream));
    if (!std::holds_alternative<StrLit>(lit)) {
        return litrb.error("Expected string literal");
    }
    litrb.commit();
    Option<Rc<IdentExpr>> as;
    if (Token::draw(Keyword::As, stream)) {
        GEODE_UNWRAP_INTO(as, IdentExpr::pull(stream));
    }
    return rb.commit<ImportExpr>(std::get<StrLit>(lit), as);
}

Type ImportExpr::typecheck(UnitParser& state) const {
    auto file = state.getSrc()->getSearchDir() / from;
    auto src = SrcFile::from(file);
    if (!src) {
        state.error(range, "{}", src.unwrapErr());
        return Type(VoidType(), nullptr);
    }
    auto parsed = UnitParser::parse(state.getShared(), src.unwrap());
    Vec<Entity> imported;
    // empty = import everything
    for (auto& ty : parsed->getAllExported()) {
        imported.push_back(ty);
    }
    if (as) {
        if (state.getEntity(as.value()->path, true)) {
            state.error(range, "Entity \"{}\" already exists in this scope", as.value()->path);
        }
        else {
            state.push(Namespace {
                .name = as.value()->path,
                .decl = nullptr,
            });
        }
    }
    for (auto& ent : imported) {
        // it shouldn't be possible for an anonymous entity to be exported
        auto name = TRY_FUN(ent.getName().value());
        if (as) {
            name = as.value()->path.joinForce(name);
        }
        if (state.getEntity(name, true)) {
            state.error(range, "Entity \"{}\" already exists in this scope", name);
        }
        else {
            state.push(ent);
        }
    }
    return Type(VoidType(), nullptr);
}

std::string ImportExpr::debug(size_t indent) const {
    return DebugPrint("ImportExpr", indent)
        .member("from", from)
        .member("as", as);
}

ExprResult<ListExpr> ListExpr::pull(Stream& stream) {
    Rollback rb(stream);
    Vec<Rc<Expr>> list;
    // handle just {}
    if (Token::peek('}', stream)) {
        return rb.commit<ListExpr>(list);
    }
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto expr, Expr::pull(stream));
        list.push_back(expr);
        // todo: yield expr if last omitted
        // Allow omitting last semicolon
        if (!Token::pullSemicolons(stream) && !Token::peek('}', stream)) {
            return rb.error("Expected semicolon");
        }
        // End at EOF or }
        if (!Token::peek(stream) || Token::peek('}', stream)) {
            break;
        }
    }
    return rb.commit<ListExpr>(list);
}

Type ListExpr::typecheck(UnitParser& state) const {
    for (auto& expr : exprs) {
        if (state.isReturning()) {
            state.error(expr->range, "Unreachable code detected");
        }
        expr->typecheck(state);
    }
    return Type(VoidType(), nullptr);
}

std::string ListExpr::debug(size_t indent) const {
    return DebugPrint("ListExpr", indent)
        .member("exprs", exprs);
}

ExprResult<ReturnExpr> ReturnExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull(Keyword::Return, stream));
    Option<Rc<Expr>> expr;
    if (!Token::peek(';', stream)) {
        GEODE_UNWRAP_INTO(expr, Expr::pull(stream));
    }
    Option<Rc<IdentExpr>> from;
    if (Token::draw(Keyword::From, stream)) {
        GEODE_UNWRAP_INTO(from, IdentExpr::pull(stream));
    }
    return rb.commit<ReturnExpr>(expr, from);
}

Type ReturnExpr::typecheck(UnitParser& state) const {
    auto ty = expr ? expr.value()->typecheck(state) : Primitive::Void;
    state.setReturnInfo(
        (from ? Option(from.value()->path) : None),
        true,
        ty
    );
    return Primitive::Void;
}

std::string ReturnExpr::debug(size_t indent) const {
    return DebugPrint("ReturnExpr", indent)
        .member("expr", expr)
        .member("from", from);
}

ExprResult<BlockExpr> BlockExpr::pull(Stream& stream) {
    Rollback rb(stream);
    Option<Rc<IdentExpr>> name;
    if (Token::draw(Keyword::Scope, stream)) {
        GEODE_UNWRAP_INTO(name, IdentExpr::pull(stream));
    }
    GEODE_UNWRAP(Token::pull('{', stream));
    GEODE_UNWRAP_INTO(auto expr, ListExpr::pull(stream));
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<BlockExpr>(expr, name);
}

Type BlockExpr::typecheck(UnitParser& state) const {
    if (name) {
        state.verifyCanPush(name.value());
        state.push(ScopeEntity {
            .name = name.value()->path,
            .decl = shared_from_this(),
        });
    }
    state.pushScope(name, false);
    auto ret = expr->typecheck(state);
    return state.popScope().value_or(ret);
}

std::string BlockExpr::debug(size_t indent) const {
    return DebugPrint("BlockExpr", indent)
        .member("expr", expr);
}

ExprResult<DebugExpr> DebugExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull('@', stream));
    GEODE_UNWRAP(Token::pull(Op::Not, stream));
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    if (ident != "debug") {
        return rb.error("Expected 'debug'");
    }
    GEODE_UNWRAP(Token::pull('(', stream));
    Rollback litrb(stream);
    GEODE_UNWRAP_INTO(auto lit, Token::pull<Lit>(stream));
    if (!std::holds_alternative<StrLit>(lit)) {
        return litrb.error("Expected string literal");
    }
    litrb.commit();
    GEODE_UNWRAP(Token::pull(')', stream));
    return rb.commit<DebugExpr>(std::get<StrLit>(lit));
}

Type DebugExpr::typecheck(UnitParser& state) const {
    switch (hash(what.c_str())) {
        case hash("entities"): {
            std::string msg = "";
            size_t i = 0;
            for (auto const& scope : state.getScopes()) {
                msg += fmt::format("Scope {}\n", i);
                for (auto const& ent : scope.getEntities()) {
                    msg += ent.getName().value_or(IdentPath("<anonymous entity>")).toString() + "\n";
                }
                i += 1;
            }
            state.log(range, "== Start of Scope Dump ==\n{}== End of Scope Dump ==", msg);
        } break;

        default: {
            state.error(range, "Invalid debug option \"{}\", valid are: \"entities\"", what);
        } break;
    }
    return Primitive::Unk;
}

std::string DebugExpr::debug(size_t indent) const {
    return DebugPrint("DebugExpr", indent)
        .member("what", what);
}

ExprResult<AST> AST::pull(Stream& stream) {
    Token::skipToNext(stream);
    Rollback rb(stream);
    Vec<Rc<Expr>> exprs;
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto expr, Expr::pull(stream));
        exprs.push_back(expr);
        GEODE_UNWRAP(Token::pullSemicolons(stream));
        if (!Token::peek(stream)) {
            break;
        }
    }
    return rb.commit<AST>(exprs);
}

Type AST::typecheck(UnitParser& state) const {
    for (auto& expr : exprs) {
        expr->typecheck(state);
    }
    return Type(VoidType(), nullptr);
}

std::string AST::debug(size_t indent) const {
    return DebugPrint("AST", indent)
        .member("exprs", exprs);
}
