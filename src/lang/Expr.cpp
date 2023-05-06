#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<TypeIdentExpr> TypeIdentExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto value, Token::pull<Ident>(stream));
    return rb.commit<TypeIdentExpr>(value);
}

TypeCheckResult TypeIdentExpr::typecheck(Rc<SrcParser> state) const {
    auto ty = state->getType(ident);
    if (!ty) {
        return state->error(range, "Unknown type \"{}\"", ident);
    }
    return Ok(*ty);
}

std::string TypeIdentExpr::debug(size_t indent) const {
    return DebugPrint("TypeIdentExpr", indent)
        .member("ident", ident);
}

ExprResult<TypeExpr> TypeExpr::pull(Stream& stream) {
    GEODE_UNWRAP_INTO(auto ty, TypeIdentExpr::pull(stream));
    return Ok(ty);
}

ExprResult<LitExpr> LitExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto value, Token::pull<Lit>(stream));
    return rb.commit<LitExpr>(value);
}

TypeCheckResult LitExpr::typecheck(Rc<SrcParser>) const {
    return Ok(std::visit(makeVisitor {
        [](VoidLit const&) {
            return Type(VoidType());
        },
        [](BoolLit const&) {
            return Type(BoolType());
        },
        [](IntLit const&) {
            return Type(IntType());
        },
        [](FloatLit const&) {
            return Type(FloatType());
        },
        [](StrLit const&) {
            return Type(StrType());
        },
    }, value));
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

TypeCheckResult IdentExpr::typecheck(Rc<SrcParser> state) const {
    auto var = state->getVar(ident);
    if (!var) {
        return state->error(range, "Unknown identifier \"{}\"", ident);
    }
    return Ok(var->type);
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

TypeCheckResult BinOpExpr::typecheck(Rc<SrcParser> state) const {
    GEODE_UNWRAP_INTO(auto r, rhs->typecheck(state));
    GEODE_UNWRAP_INTO(auto l, lhs->typecheck(state));
    if (r != l) {
        return state->error(range, "Mismatching types for binary operation");
    }
    return Ok(r);
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

TypeCheckResult MemberExpr::typecheck(Rc<SrcParser> state) const {
    GEODE_UNWRAP_INTO(auto t, target->typecheck(state));
    auto mem = t.getMemberType(member);
    if (!mem) {
        return state->error(range, "Type '{}' has no member or method named \"{}\"", t.toString(), member);
    }
    return Ok(mem.value());
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

TypeCheckResult CallExpr::typecheck(Rc<SrcParser> state) const {
    throw std::runtime_error("todo");
}

std::string CallExpr::debug(size_t indent) const {
    return DebugPrint("CallExpr", indent)
        .member("target", target)
        .member("args", args);
}

ExprResult<PropExpr> PropExpr::pull(Ident const& node, Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    GEODE_UNWRAP(Token::pull(':', stream));
    GEODE_UNWRAP_INTO(auto value, Expr::pull(stream));
    return rb.commit<PropExpr>(ident, value, node);
}

TypeCheckResult PropExpr::typecheck(Rc<SrcParser> state) const {
    auto ty = state->getType(node);
    // This has been errored in NodeExpr already
    if (!ty) {
        return Ok(Type(VoidType()));
    }
    auto mem = ty->getMemberType(prop);
    if (!mem) {
        return state->error(range, "Node or struct \"{}\" has no property \"{}\"", node, prop);
    }
    GEODE_UNWRAP_INTO(auto val, value->typecheck(state)); 
    if (mem.value() != val) {
        return state->error(
            range, "Attempted to assign '{}' to property of type '{}'",
            val.toString(), mem.value().toString()
        );
    }
    return Ok(mem.value());
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
            GEODE_UNWRAP_INTO(auto prop, PropExpr::pull(ident, stream));
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

TypeCheckResult NodeExpr::typecheck(Rc<SrcParser> state) const {
    auto ty = state->getType(ident);
    if (!ty) {
        // todo: hint that you can define nodes with decl
        return state->error(range, "Unknown node \"{}\"", ident);
    }
    for (auto& prop : props) {
        GEODE_UNWRAP(prop->typecheck(state));
    }
    for (auto& child : children) {
        GEODE_UNWRAP_INTO(auto cty, child->typecheck(state));
        // todo: check that the node type can be added as a child to this one
    }
    return Ok(*ty);
}

std::string NodeExpr::debug(size_t indent) const {
    return DebugPrint("NodeExpr", indent)
        .member("ident", ident)
        .member("props", props)
        .member("children", children);
}

ExprResult<MemberDeclExpr> MemberDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    GEODE_UNWRAP(Token::pull(':', stream));
    GEODE_UNWRAP_INTO(auto type, TypeExpr::pull(stream));
    return rb.commit<MemberDeclExpr>(ident, type);
}

TypeCheckResult MemberDeclExpr::typecheck(Rc<SrcParser> state) const {
    GEODE_UNWRAP_INTO(auto ty, type->typecheck(state));
    return Ok(ty);
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
    Vec<Rc<MemberDeclExpr>> members;
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto mem, MemberDeclExpr::pull(stream));
        members.push_back(mem);
    }
    return rb.commit<StructDeclExpr>(ident, members);
}

TypeCheckResult StructDeclExpr::typecheck(Rc<SrcParser> state) const {
    if (ident) {
        if (state->getType(ident.value())) {
            return state->error(range, "Struct type \"{}\" has already been defined", ident.value());
        }
    }
    StructType sty;
    sty.name = ident;
    for (auto& mem : members) {
        GEODE_UNWRAP_INTO(auto mty, mem->typecheck(state));
        sty.members[mem->name] = PropType {
            .type = mty,
            .defaultValue = None,
            .binding = None
        };
    }
    if (ident) {
        state->pushType(Type(sty));
    }
    return Ok(sty);
}

std::string StructDeclExpr::debug(size_t indent) const {
    return DebugPrint("StructDeclExpr", indent)
        .member("ident", ident)
        .member("members", members);
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

TypeCheckResult ListExpr::typecheck(Rc<SrcParser> state) const {
    for (auto& expr : exprs) {
        GEODE_UNWRAP(expr->typecheck(state));
    }
    // todo: return types
    return Ok(Type(VoidType()));
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

TypeCheckResult AST::typecheck(Rc<SrcParser> state) const {
    for (auto& expr : exprs) {
        GEODE_UNWRAP(expr->typecheck(state));
    }
    return Ok(Type(VoidType()));
}

std::string AST::debug(size_t indent) const {
    return DebugPrint("AST", indent)
        .member("exprs", exprs);
}
