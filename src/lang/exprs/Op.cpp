#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "../Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

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

Type BinOpExpr::typecheck(UnitParser& state) const {
    auto l = lhs->typecheck(state);
    auto r = rhs->typecheck(state);
    if (!r.convertible(l)) {
        state.error(range, "Mismatching types for binary operation")
            .note("Left side is {}", l.toString())
            .note("Right side is {}", r.toString());
    }
    return r;
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

Type MemberExpr::typecheck(UnitParser& state) const {
    auto t = target->typecheck(state);
    auto mem = t.getMemberType(member);
    if (!mem) {
        state.error(range, "Type '{}' has no member or method named \"{}\"", t.toString(), member);
    }
    return mem.value();
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
        GEODE_UNWRAP_INTO(auto brk, Token::pullSeparator(',', ')', stream));
        if (brk) {
            break;
        }
    }
    GEODE_UNWRAP(Token::pull(')', stream));
    return rb.commit<CallExpr>(target, args);
}

Type CallExpr::typecheck(UnitParser& state) const {
    throw std::runtime_error("todo");
}

std::string CallExpr::debug(size_t indent) const {
    return DebugPrint("CallExpr", indent)
        .member("target", target)
        .member("args", args);
}
