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
        [](StructType const& str) -> std::string {
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
                ret += mem + ": " + ty.type.toString();
            }
            ret += " }";
            return ret;
        },
        [](NodeType const& node) -> std::string {
            return node.name;
        },
    }, kind);
}

Option<Type> Type::getMemberType(std::string const& name) const {
    return std::visit(makeVisitor {
        [&](VoidType const&) -> Option<Type> {
            return None;
        },
        [&](BoolType const&) -> Option<Type> {
            return None;
        },
        [&](IntType const&) -> Option<Type> {
            return None;
        },
        [&](FloatType const&) -> Option<Type> {
            return None;
        },
        [&](StrType const&) -> Option<Type> {
            return None;
        },
        [&](StructType const& str) -> Option<Type> {
            if (str.members.contains(name)) {
                return str.members.at(name).type;
            }
            return None;
        },
        [&](NodeType const& node) -> Option<Type> {
            if (node.props.contains(name)) {
                return node.props.at(name).type;
            }
            return None;
        },
    }, kind);
}

Type Value::getType() const {
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
        [](auto const& str) {
            return Type(str.type);
        },
    }, kind);
}
