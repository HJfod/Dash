#include <lang/Type.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

bool Type::equal(Type const& other) const {
    // unknown types are always equal to everything else
    if (
        std::holds_alternative<UnkType>(kind) ||
        std::holds_alternative<UnkType>(other.kind)
    ) {
        return true;
    }
    return kind.index() == other.kind.index();
}

bool Type::convertible(Type const& other) const {
    if (this->equal(other)) {
        return true;
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
        [](UnkType const&) -> std::string {
            return "unknown";
        },
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
                ret += mem + ": " + ty.type->toString();
            }
            ret += " }";
            return ret;
        },
        [](NodeType const& node) -> std::string {
            return node.name;
        },
        [](RefType const& ref) -> std::string {
            return fmt::format("&{}", ref.type->toString());
        },
    }, kind);
}

Option<Type> Type::getMemberType(std::string const& name) const {
    return std::visit(makeVisitor {
        [&](StructType const& str) -> Option<Type> {
            if (str.members.contains(name)) {
                return str.members.at(name).type.clone();
            }
            return None;
        },
        [&](NodeType const& node) -> Option<Type> {
            if (node.props.contains(name)) {
                return node.props.at(name).type.clone();
            }
            return None;
        },
        [&](RefType const& ref) -> Option<Type> {
            return ref.type->getMemberType(name);
        },
        [&](auto const&) -> Option<Type> {
            return Type(UnkType());
        },
    }, kind);
}

Set<String> Type::getRequiredMembers() const {
    return std::visit(makeVisitor {
        [](StructType const& str) {
            Set<String> ret;
            for (auto& [name, ty] : str.members) {
                if (ty.required) {
                    ret.insert(name);
                }
            }
            return ret;
        },
        [](NodeType const& node) {
            Set<String> ret;
            for (auto& [name, ty] : node.props) {
                if (ty.required) {
                    ret.insert(name);
                }
            }
            return ret;
        },
        [&](RefType const& ref) {
            return ref.type->getRequiredMembers();
        },
        [](auto const&) -> Set<String> {
            return {};
        },
    }, kind);
}

bool Type::isExportable() const {
    return std::visit(makeVisitor {
        [](StructType const& str) {
            return str.name.has_value();
        },
        [](NodeType const& node) {
            return true;
        },
        [](auto const&) {
            return false;
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
