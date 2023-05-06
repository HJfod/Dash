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
    }, value);
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

Type IdentExpr::typecheck(UnitParser& state) const {
    auto var = state.getVar(ident);
    if (!var) {
        state.error(range, "Unknown identifier \"{}\"", ident);
        return Type(UnkType());
    }
    return var->type;
}

std::string IdentExpr::debug(size_t indent) const {
    return DebugPrint("IdentExpr", indent)
        .member("ident", ident);
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

Type NodeExpr::typecheck(UnitParser& state) const {
    if (ident) {
        auto ty = state.getType(ident.value());
        if (!ty) {
            // todo: hint that you can define nodes with decl
            state.error(range, "Unknown node or struct \"{}\"", ident);
            return Type(UnkType());
        }
        Set<Ident> assigned;
        for (auto& prop : props) {
            auto valty = prop->typecheck(state);
            auto mem = ty->getMemberType(prop->prop);
            if (!mem) {
                state.error(range, "Node or struct \"{}\" has no property \"{}\"", ident.value(), prop->prop);
                return Type(UnkType());
            }
            if (mem.value() != valty) {
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
            if (!std::holds_alternative<NodeType>(ty->kind)) {
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
                    .defaultValue = None,
                    .dependencies = {},
                    .opaque = false,
                }});
            }
        }
        if (children.size()) {
            state.error(range, "Anonymous structs may not contain children");
        }
        for (auto& child : children) {
            child->typecheck(state);
        }
        return Type(ty);
    }
}

std::string NodeExpr::debug(size_t indent) const {
    return DebugPrint("NodeExpr", indent)
        .member("ident", ident)
        .member("props", props)
        .member("children", children);
}
