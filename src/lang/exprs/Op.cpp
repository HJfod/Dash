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
    if (isOverloadableOp(op)) {
        auto fun = state.getFun(IdentPath(opFunName(op, l, r)));
        if (!fun) {
            state.error(range, "Undefined types for binary operation `{}`", tokenToString(op))
                .note("Left side is `{}`", l.toString())
                .note("Right side is `{}`", r.toString());
            return Primitive::Unk;
        }
        return fun->type.retType.value_or(Type(Primitive::Unk));
    }
    else {
        if (op == Op::Seq) {
            if (!l.convertible(r)) {
                state.error(range, "Mismatching types for assignment")
                    .note("Left side is `{}`", l.toString())
                    .note("Right side is `{}`", r.toString());
            }
            return l;
        }

        if (op == Op::Neq) {
            auto fun = state.getFun(IdentPath(opFunName(Op::Eq, l, r)));
            if (!fun) {
                state.error(range, "Undefined types for binary operation")
                    .note("Left side is `{}`", l.toString())
                    .note("Right side is `{}`", r.toString())
                    .note("Operator `{}` not defined for these types", tokenToString(Op::Eq));
                return Primitive::Unk;
            }
            return fun->type.retType.value_or(Type(Primitive::Unk));
        }

        if (op == Op::Leq) {
            auto lfun = state.getFun(IdentPath(opFunName(Op::Less, l, r)));
            if (!lfun) {
                state.error(range, "Undefined types for binary operation `{}`", tokenToString(Op::Leq))
                    .note("Left side is `{}`", l.toString())
                    .note("Right side is `{}`", r.toString())
                    .note("Missing definition of `{}`", tokenToString(Op::Less));
                return Primitive::Unk;
            }
            auto efun = state.getFun(IdentPath(opFunName(Op::Eq, l, r)));
            if (!efun) {
                state.error(range, "Undefined types for binary operation `{}`", tokenToString(Op::Leq))
                    .note("Left side is `{}`", l.toString())
                    .note("Right side is `{}`", r.toString())
                    .note("Missing definition of `{}`", tokenToString(Op::Eq));
                return Primitive::Unk;
            }
            auto lret = lfun->type.retType.value_or(Type(Primitive::Unk));
            auto eret = efun->type.retType.value_or(Type(Primitive::Unk));
            if (!lret->convertible(eret)) {
                state.error(
                    range,
                    "Mismatching equality and less-than operator return types for `{}` and `{}`",
                    l.toString(), r.toString()
                )
                    .note("Less-than operator returns `{}`", lret->toString())
                    .note("Equality operator returns `{}`", eret->toString());
            }
            return lret;
        }

        if (isBoolOp(op)) {
            if (!state.getFun(IdentPath(asFunName(l, Primitive::Bool)))) {
                state.error(range, "Left hand side `{}` is not convertible to bool", l.toString());
                return Primitive::Unk;
            }
            if (!state.getFun(IdentPath(asFunName(r, Primitive::Bool)))) {
                state.error(range, "Right hand side `{}` is not convertible to bool", r.toString());
                return Primitive::Unk;
            }
            return Primitive::Bool;
        }

        auto top = seqOpSynthesisSrc(op);
        if (top != op) {
            auto fun = state.getFun(IdentPath(opFunName(top, l, r)));
            if (!fun) {
                state.error(range, "Undefined types for assignment `{}`", tokenToString(top))
                    .note("Left side is `{}`", l.toString())
                    .note("Right side is `{}`", r.toString());
                return Primitive::Unk;
            }
            auto ret = fun->type.retType.value_or(Type(Primitive::Unk));
            if (!ret->convertible(l)) {
                state.error(range, "Result of operation not assignable")
                    .note("Operation results in `{}`", ret->toString())
                    .note("Attempted to assign to `{}`", l.toString());
            }
            return ret;
        }

        throw std::runtime_error(fmt::format("Undefined binop {}", tokenToString(op)));
    }
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
        return Primitive::Unk;
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
