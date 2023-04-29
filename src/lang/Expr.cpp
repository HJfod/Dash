#include <lang/Expr.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<LitExpr> LitExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto value, Token::pull<Lit>(stream));
    return rb.commit<LitExpr>(value);
}

ExprResult<IdentExpr> IdentExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto value, Token::pull<Ident>(stream));
    return rb.commit<IdentExpr>(value);
}

ExprResult<Expr> BinOpExpr::pull(Stream& stream, size_t prec, Rc<Expr> lhs) {
    Rollback rb(stream);
    while (true) {
        stream.debugTick();
        auto nextOp = Token::peek<Op>(stream);
        if (!nextOp) {
            break;
        }
        auto op = nextOp.value();

        // If next op binds less tightly, we are done
        auto pop = opPriority(op);
        if (pop < prec) {
            return Ok(lhs);
        }

        // Consume op
        (void)Token::pull<Op>(stream);

        GEODE_UNWRAP_INTO(auto rhs, Expr::pullPrimary(stream));
        if (opDir(op) == OpDir::RTL) {
            GEODE_UNWRAP_INTO(rhs, BinOpExpr::pull(stream, prec, rhs));
        }
        nextOp = Token::peek<Op>(stream);
        // If next op binds more tightly, it gets to be RHS
        if (nextOp && pop < opPriority(nextOp.value())) {
            GEODE_UNWRAP_INTO(rhs, BinOpExpr::pull(stream, pop + 1, rhs));
        }

        lhs = rb.commit<BinOpExpr>(lhs, rhs, op).unwrap();
    }
    return Ok(lhs);
}

ExprResult<Expr> BinOpExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto lhs, Expr::pullPrimary(stream));
    rb.commit();
    if (Token::peek<Op>(stream)) {
        return BinOpExpr::pull(stream, 0, lhs);
    }
    return Ok(lhs);
}

ExprResult<CallExpr> CallExpr::pull(Rc<Expr> target, Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull('(', stream));
    // handle ()
    if (Token::pull(')', stream)) {
        return rb.commit<CallExpr>(target, Vec<Rc<Expr>>());
    }
    Vec<Rc<Expr>> args;
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto arg, Expr::pull(stream));
        args.push_back(arg);
        if (Token::peek(')', stream)) {
            break;
        }
        GEODE_UNWRAP(Token::pull(',', stream));
        // Allow trailing comma
        if (Token::peek(')', stream)) {
            break;
        }
    }
    GEODE_UNWRAP(Token::pull(')', stream));
    return rb.commit<CallExpr>(target, args);
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
        // Allow omitting last semicolon
        if (!Token::peek(';', stream) && !Token::peek('}', stream)) {
            return rb.error("Expected semicolon");
        }
        // Allow any number of semicolons
        while (Token::pull(';', stream)) {}
        // End at EOF or }
        if (!Token::peek(stream) || Token::peek('}', stream)) {
            break;
        }
    }
    return rb.commit<ListExpr>(list);
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
        if (Token::peek('(', stream)) {
            GEODE_UNWRAP_INTO(expr, CallExpr::pull(expr, stream));
            continue;
        }
        break;
    }
    rb.commit();
    return Ok(expr);
}

#define TRY_PULL(expr) \
    if (auto value = expr::pull(stream)) {\
        rb.commit(); \
        return value; \
    }
    
ExprResult<Expr> Expr::pullPrimaryNonCall(Stream& stream) {
    Rollback rb(stream);

    // Parenthesis
    if (Token::pull('(', stream)) {
        GEODE_UNWRAP_INTO(auto expr, Expr::pull(stream));
        GEODE_UNWRAP(Token::pull(')', stream));
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

