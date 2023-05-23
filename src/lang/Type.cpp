#include <lang/Type.hpp>
#include "Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

IdentPath::IdentPath() = default;
IdentPath::IdentPath(Ident const& name) : name(name) {}

bool IdentPath::operator==(IdentPath const& other) const = default;

std::string IdentPath::toString() const {
    std::string start = absolute ? "::" : "";
    if (path.empty()) {
        return start + name;
    }
    return start + fmt::format("{}::{}", fmt::join(path, "::"), name);
}

bool IdentPath::isSingle() const {
    return path.empty() && !absolute;
}

Vec<Ident> IdentPath::getComponents() const {
    auto ret = path;
    ret.push_back(name);
    return ret;
}

Option<IdentPath> IdentPath::getParent() const {
    if (this->path.size()) {
        auto nuevo = IdentPath();
        nuevo.absolute = this->absolute;
        nuevo.name = this->path.back();
        nuevo.path = this->path;
        nuevo.path.pop_back();
        return nuevo;
    }
    return None;
}

FullIdentPath::FullIdentPath(Vec<Ident> const& components) : path(components) {}

FullIdentPath::FullIdentPath(IdentPath const& path) : path(path.path) {
    this->path.push_back(path.name);
}

bool FullIdentPath::operator==(FullIdentPath const& other) const = default;

std::string FullIdentPath::toString() const {
    return fmt::format("::{}", fmt::join(path, "::"));
}

Option<FullIdentPath> FullIdentPath::resolve(IdentPath const& path, bool existing) const {
    if (path.absolute) {
        if (existing ? FullIdentPath(path) == *this : FullIdentPath(path.path) == *this) {
            return FullIdentPath(path);
        }
        else {
            return None;
        }
    }
    auto comps = existing ? path.getComponents() : path.path;
    if (comps.size() > this->path.size()) {
        return None;
    }
    for (size_t i = comps.size(); i > 0; i--) {
        if (this->path[i - 1] != comps[i - 1]) {
            return None;
        }
    }
    auto ret = this->path;
    ret.erase(ret.begin() + (this->path.size() - comps.size()), ret.end());
    ranges::push(ret, comps);
    if (!existing) {
        ret.push_back(path.name);
    }
    return ret;
}

FullIdentPath FullIdentPath::join(Ident const& component) const {
    auto comps = path;
    comps.push_back(component);
    return comps;
}

FullIdentPath FullIdentPath::join(IdentPath const& components) const {
    auto comps = path;
    if (!components.absolute) {
        for (auto& comp : components.path) {
            comps.push_back(comp);
        }
        comps.push_back(components.name);
    }
    return comps;
}

Type::Type(Value const& value, Rc<const Expr> decl)
  : kind(value), decl(decl) {}

Type::Type(Primitive type, std::source_location const loc) {
    this->decl = nullptr;
    switch (type) {
        case Primitive::Unk: this->kind = UnkType(); break;
        case Primitive::Void: this->kind = VoidType(); break;
        case Primitive::Bool: this->kind = BoolType(); break;
        case Primitive::Int: this->kind = IntType(); break;
        case Primitive::Float: this->kind = FloatType(); break;
        case Primitive::Str: this->kind = StrType(); break;
        default: throw std::runtime_error(
            fmt::format("Invalid built-in type {} (from {})",
            static_cast<int>(type), loc
        ));
    }
}

Type Type::realize() const {
    return std::visit(makeVisitor {
        [](AliasType const& alias) {
            return alias.type->realize();
        },
        [](RefType const& ref) {
            return ref.type->realize();
        },
        [&](auto const&) {
            return *this;
        }
    }, kind);
}

bool Type::convertible(Type const& other) const {
    auto from = this->realize();
    auto into = other.realize();
    // unknown types are always equal to everything else
    if (
        std::holds_alternative<UnkType>(from.kind) ||
        std::holds_alternative<UnkType>(into.kind)
    ) {
        return true;
    }
    if (from.kind.index() != into.kind.index()) {
        return false;
    }
    if (auto str = std::get_if<StructType>(&from.kind)) {
        // must have all the same members as into, can have extra members tho
        for (auto& [name, mem] : std::get<StructType>(into.kind).members) {
            if (
                !str->members.contains(name) ||
                !str->members.at(name).type->convertible(mem.type)
            ) {
                return false;
            }
        }
        return true;
    }
    if (auto str = std::get_if<NodeType>(&from.kind)) {
        return str->name == std::get<NodeType>(into.kind).name;
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
        [](FunType const& fun) -> std::string {
            if (fun.name) {
                return fun.name->toString();
            }
            std::string ret = "fun (";
            bool first = true;
            for (auto& param : fun.params) {
                if (!first) {
                    ret += ", ";
                }
                first = false;
                ret += param.name + ": " + param.type->toString();
            }
            if (fun.retType) {
                ret += ") -> " + fun.retType.value()->toString();
            }
            else {
                ret += ") -> auto";
            }
            return ret;
        },
        [](StructType const& str) -> std::string {
            if (str.name) {
                return str.name->toString();
            }
            if (!str.members.size()) {
                return "struct {}";
            }
            std::string ret = "struct { ";
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
        [](EnumType const& str) -> std::string {
            if (str.name) {
                return str.name->toString();
            }
            if (!str.variants.size()) {
                return "enum {}";
            }
            std::string ret = "enum { ";
            bool first = true;
            for (auto& [mem, ty] : str.variants) {
                if (!first) {
                    ret += ", ";
                }
                first = false;
                ret += mem + ": " + ty.toString();
            }
            ret += " }";
            return ret;
        },
        [](NodeType const& node) -> std::string {
            return node.name.toString();
        },
        [](RefType const& ref) -> std::string {
            return fmt::format("&{}", ref.type->toString());
        },
        [](AliasType const& alias) -> std::string {
            return alias.alias.toString();
        },
    }, kind);
}

Option<IdentPath> Type::getName() const {
    return std::visit(makeVisitor {
        [](StructType const& str) {
            return str.name;
        },
        [](EnumType const& str) {
            return str.name;
        },
        [](FunType const& fun) {
            return fun.name;
        },
        [](NodeType const& node) -> Option<IdentPath> {
            return node.name;
        },
        [](VoidType const&) -> Option<IdentPath> {
            return IdentPath("void");
        },
        [](BoolType const&) -> Option<IdentPath> {
            return IdentPath("bool");
        },
        [](IntType const&) -> Option<IdentPath> {
            return IdentPath("int");
        },
        [](FloatType const&) -> Option<IdentPath> {
            return IdentPath("float");
        },
        [](StrType const&) -> Option<IdentPath> {
            return IdentPath("string");
        },
        [](RefType const&) -> Option<IdentPath> {
            return None;
        },
        [](AliasType const& alias) -> Option<IdentPath> {
            return alias.alias;
        },
        [](UnkType const&) -> Option<IdentPath> {
            return None;
        },
    }, kind);
}

Rc<const Expr> Type::getDecl() const {
    return this->decl;
}

Option<Type> Type::getReturnType() const {
    return std::visit(makeVisitor {
        [&](FunType const& fun) -> Option<Type> {
            if (fun.retType) {
                return fun.retType.value().clone();
            }
            return None;
        },
        [&](auto const&) -> Option<Type> {
            return None;
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
            return Primitive::Unk;
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
        [](VoidLit const&) -> Type {
            return Primitive::Void;
        },
        [](BoolLit const&) -> Type {
            return Primitive::Bool;
        },
        [](IntLit const&) -> Type {
            return Primitive::Int;
        },
        [](FloatLit const&) -> Type {
            return Primitive::Float;
        },
        [](StrLit const&) -> Type {
            return Primitive::Str;
        },
        [](auto const& str) {
            return Type(str.type, nullptr);
        },
    }, kind);
}

std::string lang::opFunName(Op op, Type a) {
    return fmt::format("[{} {}]", tokenToString(op), a.toString());
}

std::string lang::opFunName(Op op, Type a, Type b) {
    return fmt::format("[{} {} {}]", a.toString(), tokenToString(op), b.toString());
}

std::string lang::asFunName(Type a, Type b) {
    return fmt::format("[{} as {}]", a.toString(), b.toString());
}

std::size_t std::hash<IdentPath>::operator()(IdentPath const& path) const noexcept {
    return std::hash<Ident>()(path.toString());
}

std::size_t std::hash<FullIdentPath>::operator()(FullIdentPath const& path) const noexcept {
    return std::hash<Ident>()(path.toString());
}
