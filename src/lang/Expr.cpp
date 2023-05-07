#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

#define TRY_PULL(expr) \
    if (auto value = expr::pull(stream)) {\
        rb.commit(); \
        return value; \
    }\
    rb.clearMessages();

#define PULL_IF(expr, cond)                                 \
    if (Token::peek(cond, stream)) {                        \
        GEODE_UNWRAP_INTO(auto value, expr::pull(stream));  \
        rb.commit();                                        \
        return Ok(value);                                   \
    }

ExprResult<Expr> Expr::pull(Stream& stream) {
    return BinOpExpr::pull(stream);
}

ExprResult<Expr> Expr::pullPrimary(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto expr, Expr::pullPrimaryNonCall(stream));
    // Get all indexes and calls (if chained)
    while (true) {
        stream.debugTick();
        if (Token::peek('.', stream)) {
            GEODE_UNWRAP_INTO(expr, MemberExpr::pull(expr, stream));
            continue;
        }
        if (Token::peek('(', stream)) {
            GEODE_UNWRAP_INTO(expr, CallExpr::pull(expr, stream));
            continue;
        }
        break;
    }
    rb.commit();
    return Ok(expr);
}

ExprResult<Expr> Expr::pullPrimaryNonCall(Stream& stream) {
    Rollback rb(stream);

    // Parenthesis
    if (Token::draw('(', stream)) {
        GEODE_UNWRAP_INTO(auto expr, Expr::pull(stream));
        GEODE_UNWRAP(Token::pull(')', stream));
        rb.commit();
        return Ok(expr);
    }

    PULL_IF(BlockExpr, '{');
    PULL_IF(VarDeclExpr, Keyword::Let);
    PULL_IF(ImportExpr, Keyword::Import);
    PULL_IF(ExportExpr, Keyword::Export);
    PULL_IF(StructDeclExpr, Keyword::Struct);
    if (Token::peek<Ident>(stream) && Token::peek('{', stream, 1)) {
        GEODE_UNWRAP_INTO(auto expr, NodeExpr::pull(stream));
        rb.commit();
        return Ok(expr);
    }
    TRY_PULL(LitExpr);
    TRY_PULL(IdentExpr);

    auto token = Token::pull(stream);
    if (token) {
        return rb.error("Expected expression, got '{}'", token.unwrap().toString());
    }
    else {
        return Err(token.unwrapErr());
    }
}
