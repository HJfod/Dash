#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "../Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<LitExpr> LitExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto value, Token::pull<Lit>(stream));
    return rb.commit<LitExpr>(value);
}

Type LitExpr::typecheck(UnitParser&) const {
    return std::visit(makeVisitor {
        [](VoidLit const&) {
            return Primitive::Void;
        },
        [](BoolLit const&) {
            return Primitive::Bool;
        },
        [](IntLit const&) {
            return Primitive::Int;
        },
        [](FloatLit const&) {
            return Primitive::Float;
        },
        [](StrLit const&) {
            return Primitive::Str;
        },
    }, value);
}

std::string LitExpr::debug(size_t indent) const {
    return DebugPrint("LitExpr", indent)
        .member("value", value);
}

ExprResult<IdentExpr> IdentExpr::pull(Stream& stream) {
    Rollback rb(stream);
    IdentPath path;
    path.absolute = Token::draw(Op::Scope, stream).has_value();
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto value, Token::pull<Ident>(stream));
        path.path.push_back(value);
        if (!Token::draw(Op::Scope, stream)) {
            break;
        }
    }
    if (path.path.empty()) {
        return rb.error("Expected identifier");
    }
    path.name = path.path.back();
    path.path.pop_back();
    return rb.commit<IdentExpr>(path);
}

Type IdentExpr::typecheck(UnitParser& state) const {
    auto var = state.getVar(path);
    if (!var) {
        state.error(range, "Unknown identifier \"{}\"", path);
        return Primitive::Unk;
    }
    return var->type;
}

std::string IdentExpr::debug(size_t indent) const {
    return DebugPrint("IdentExpr", indent)
        .member("path", path);
}

ExprResult<PropExpr> PropExpr::pull(Stream& stream) {
    Rollback rb(stream);
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    GEODE_UNWRAP(Token::pull(':', stream));
    GEODE_UNWRAP_INTO(auto value, Expr::pull(stream));
    return rb.commit<PropExpr>(ident, value);
}

Type PropExpr::typecheck(UnitParser& state) const {
    return value->typecheck(state); 
}

std::string PropExpr::debug(size_t indent) const {
    return DebugPrint("PropExpr", indent)
        .member("prop", prop)
        .member("value", value);
}

ExprResult<NodeExpr> NodeExpr::pull(Stream& stream) {
    Rollback rb(stream);
    auto ident = IdentExpr::pull(stream).ok();
    rb.clearMessages();
    GEODE_UNWRAP(Token::pull('{', stream));
    Vec<Rc<PropExpr>> props;
    Vec<Rc<NodeExpr>> children;
    while (true) {
        stream.debugTick();
        if (Token::peek<Ident>(stream) && Token::peek(':', stream, 1)) {
            GEODE_UNWRAP_INTO(auto prop, PropExpr::pull(stream));
            // allow either ; or , as separator
            if (!Token::peek('}', stream) && !Token::draw(',', stream)) {
                GEODE_UNWRAP(Token::pullSemicolons(stream));
            }
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

Type NodeExpr::typecheck(UnitParser& state) const {
    if (ident) {
        auto ty = state.getType(ident.value()->path);
        if (!ty) {
            // todo: hint that you can define nodes with decl
            state.error(ident.value()->range, "Unknown node or struct \"{}\"", ident.value()->path);
            return Primitive::Unk;
        }
        Set<Ident> assigned;
        for (auto& prop : props) {
            auto valty = prop->typecheck(state);
            auto mem = ty->getMemberType(prop->prop);
            if (!mem) {
                state.error(range, "Node or struct \"{}\" has no property \"{}\"", ident.value()->path, prop->prop);
                return Primitive::Unk;
            }
            if (!mem.value().convertible(valty)) {
                state.error(
                    range, "Attempted to assign '{}' to property of type '{}'",
                    valty.toString(), mem.value().toString()
                );
            }
            if (assigned.contains(prop->prop)) {
                state.error(prop->range, "Property or member \"{}\" has already been assigned", prop->prop);
            }
            assigned.insert(prop->prop);
        }
        for (auto& mem : ty->getRequiredMembers()) {
            if (!assigned.contains(mem)) {
                state.error(range, "Property or member \"{}\" must be assigned", mem);
            }
        }
        for (auto& child : children) {
            if (!ty->template has<NodeType>()) {
                state.error(child->range, "Only nodes may contain children");
            }
            child->typecheck(state);
            // todo: check that the node type can be added as a child to this one
        }
        return *ty;
    }
    else {
        StructType ty;
        ty.name = None;
        for (auto& prop : props) {
            auto valty = prop->typecheck(state);
            if (ty.members.contains(prop->prop)) {
                state.error(prop->range, "Member \"{}\" has already been assigned", prop->prop);
            }
            else {
                ty.members.insert({ prop->prop, PropType {
                    .type = valty,
                    .dependencies = {},
                    .required = true,
                }});
            }
        }
        if (children.size()) {
            state.error(range, "Anonymous structs may not contain children");
        }
        for (auto& child : children) {
            child->typecheck(state);
        }
        return Type(ty, shared_from_this());
    }
}

std::string NodeExpr::debug(size_t indent) const {
    return DebugPrint("NodeExpr", indent)
        .member("ident", ident)
        .member("props", props)
        .member("children", children);
}
