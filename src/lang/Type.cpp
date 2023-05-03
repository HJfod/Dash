#include <lang/Type.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

bool Type::operator==(Type const& other) const {
    if (kind.index() != other.kind.index()) {
        return false;
    }
    if (auto str = std::get_if<StructType>(&kind)) {
        return str->members == std::get<StructType>(other.kind).members;
    }
    if (auto str = std::get_if<NodeType>(&kind)) {
        return str->name == std::get<NodeType>(other.kind).name;
    }
    return true;
}

std::string Type::toString() const {
    return std::visit(makeVisitor {
        [](VoidType const&) -> std::string {
            return "void";
        },
        [](BoolType const&) -> std::string {
            return "bool";
        },
        [](IntType const&) -> std::string {
            return "int";
        },
        [](FloatType const&) -> std::string {
            return "float";
        },
        [](StrType const&) -> std::string {
            return "string";
        },
        [](StructType const& str) {
            if (str.name) {
                return str.name.value();
            }
            if (!str.members.size()) {
                return "{}";
            }
            std::string ret = "{ ";
            bool first = true;
            for (auto& [mem, ty] : str.members) {
                if (!first) {
                    ret += ", ";
                }
                first = false;
                ret += mem + ": " + ty.toString();
            }
            ret += " }";
            return ret;
        },
        [](NodeType const& node) {
            return node.name;
        },
    }, kind);
}

State::State() : m_scopes({ Scope() }) {}

void State::pushType(Type const& type) {
    m_scopes.back().types.insert({ type.toString(), type });
}

Type* State::getType(std::string const& name, bool topOnly) {
    // Prefer topmost scope
    for (auto& scope : ranges::reverse(m_scopes)) {
        if (scope.types.contains(name)) {
            return &scope.types.at(name);
        }
        if (topOnly) {
            return nullptr;
        }
    }
    return nullptr;
}

void State::pushVar(Var const& var) {
    m_scopes.back().vars.insert({ var.name, var });
}

Var* State::getVar(std::string const& name, bool topOnly) {
    // Prefer topmost scope
    for (auto& scope : ranges::reverse(m_scopes)) {
        if (scope.vars.contains(name)) {
            return &scope.vars.at(name);
        }
        if (topOnly) {
            return nullptr;
        }
    }
    return nullptr;
}

void State::pushScope() {
    m_scopes.emplace_back();
}

void State::popScope() {
    m_scopes.pop_back();
    if (m_scopes.empty()) {
        throw std::runtime_error("Internal Compiler Error: Scope stack is empty");
    }
}
