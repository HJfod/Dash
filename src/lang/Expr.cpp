#include "Expr.hpp"

#define MAKE_EXPR(ty, ...) \
    Ok(std::make_shared<ty>(__VA_ARGS__, Range(start, stream.location())))

ExprResult<LitExpr> LitExpr::pull(Stream& stream) {
    auto start = stream.location();
    GEODE_UNWRAP_INTO(auto value, Token::pull<Lit>(stream));
    return MAKE_EXPR(LitExpr, value);
}

ExprResult<IdentExpr> IdentExpr::pull(Stream& stream) {
    auto start = stream.location();
    GEODE_UNWRAP_INTO(auto value, Token::pull<Ident>(stream));
    return MAKE_EXPR(IdentExpr, value);
}

ExprResult<ListExpr> ListExpr::pull(Stream& stream) {
    auto start = stream.location();
    Vec<Rc<Expr>> list;
    // handle just {}
    if (Token::peek('}', stream)) {
        return MAKE_EXPR(ListExpr, list);
    }
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto expr, Expr::pull(stream));
        list.push_back(expr);
    }
}
