#include <lang/Expr.hpp>
#include "Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<LitExpr> LitExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto value, Token::pull<Lit>(stream));
    return rb.commit<LitExpr>(value);
}

std::string LitExpr::debug(size_t indent) const {
    return DebugPrint("LitExpr", indent)
        .member("value", value);
}

ExprResult<IdentExpr> IdentExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto value, Token::pull<Ident>(stream));
    return rb.commit<IdentExpr>(value);
}

std::string IdentExpr::debug(size_t indent) const {
    return DebugPrint("IdentExpr", indent)
        .member("ident", ident);
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

std::string BinOpExpr::debug(size_t indent) const {
    return DebugPrint("BinOpExpr", indent)
        .member("lhs", lhs)
        .member("rhs", rhs)
        .member("op", op);
}

ExprResult<MemberExpr> MemberExpr::pull(Rc<Expr> target, Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP(Token::pull('.', stream));
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    return rb.commit<MemberExpr>(target, ident);
}

std::string MemberExpr::debug(size_t indent) const {
    return DebugPrint("MemberExpr", indent)
        .member("target", target)
        .member("member", member);
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

std::string CallExpr::debug(size_t indent) const {
    return DebugPrint("CallExpr", indent)
        .member("target", target)
        .member("args", args);
}

ExprResult<PropExpr> PropExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    GEODE_UNWRAP(Token::pull(':', stream));
    GEODE_UNWRAP_INTO(auto value, Expr::pull(stream));
    return rb.commit<PropExpr>(ident, value);
}

std::string PropExpr::debug(size_t indent) const {
    return DebugPrint("PropExpr", indent)
        .member("prop", prop)
        .member("value", value);
}

ExprResult<NodeExpr> NodeExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    GEODE_UNWRAP(Token::pull('{', stream));
    Vec<Rc<PropExpr>> props;
    Vec<Rc<NodeExpr>> children;
    while (true) {
        stream.debugTick();
        if (Token::peek<Ident>(stream) && Token::peek(':', stream, 1)) {
            GEODE_UNWRAP_INTO(auto prop, PropExpr::pull(stream));
            GEODE_UNWRAP(Token::pullSemicolons(stream));
            props.push_back(prop);
        }
        else {
            GEODE_UNWRAP_INTO(auto child, NodeExpr::pull(stream));
            children.push_back(child);
        }
        if (Token::peek('}', stream)) break;
    }
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<NodeExpr>(ident, props, children);
}

std::string NodeExpr::debug(size_t indent) const {
    return DebugPrint("NodeExpr", indent)
        .member("ident", ident)
        .member("props", props)
        .member("children", children);
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

std::string ListExpr::debug(size_t indent) const {
    return DebugPrint("ListExpr", indent)
        .member("exprs", exprs);
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

#define TRY_PULL(expr) \
    if (auto value = expr::pull(stream)) {\
        rb.commit(); \
        return value; \
    }\
    rb.clearMessages();
    
ExprResult<Expr> Expr::pullPrimaryNonCall(Stream& stream) {
    Rollback rb(stream);

    // Parenthesis
    if (Token::pull('(', stream)) {
        GEODE_UNWRAP_INTO(auto expr, Expr::pull(stream));
        GEODE_UNWRAP(Token::pull(')', stream));
        rb.commit();
        return Ok(expr);
    }
    rb.clearMessages();

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

ExprResult<AST> AST::pull(Stream& stream) {
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

std::string AST::debug(size_t indent) const {
    return DebugPrint("AST", indent)
        .member("exprs", exprs);
}
