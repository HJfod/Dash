#include "Type.hpp"
#include "Compiler.hpp"
#include "Instance.hpp"
#include "GDML.hpp"

using namespace gdml;

Type::Type(Compiler& compiler, const types::DataType type)
 : m_compiler(compiler), m_type(type) {}

const types::DataType Type::getType() const {
    return m_type;
}

Value* Type::instantiate() {
    #define INSTANTIATE_TYPE(t) \
        case types::DataType::t: return new BuiltInValue<types::t>(\
            m_compiler, types::t()\
        )

    switch (m_type) {
        default: return nullptr;
        INSTANTIATE_TYPE(Bool);
        INSTANTIATE_TYPE(I8);
        INSTANTIATE_TYPE(I16);
        INSTANTIATE_TYPE(I32);
        INSTANTIATE_TYPE(I64);
        INSTANTIATE_TYPE(U8);
        INSTANTIATE_TYPE(U16);
        INSTANTIATE_TYPE(U32);
        INSTANTIATE_TYPE(U64);
        INSTANTIATE_TYPE(F32);
        INSTANTIATE_TYPE(F64);
        INSTANTIATE_TYPE(Char);
        INSTANTIATE_TYPE(String);
    }
}

std::string Type::codegenName() const {
    return types::dataTypeToCppType(m_type);
}

std::string Type::toString() const {
    return types::dataTypeToString(m_type);
}

bool Type::convertibleTo(std::shared_ptr<Type> other) const {
    return m_type == other->m_type;
}

bool Type::castableTo(std::shared_ptr<Type> other) const {
    return m_casts.count(other->toString());
}

std::string Type::getCastOperatorFor(std::shared_ptr<Type> type) const {
    return m_casts.at(type->toString());
}

void Type::addCastOperatorFor(std::shared_ptr<Type> type, std::string const& op) {
    m_casts.insert({ type->toString(), op });
}


QualifiedType::QualifiedType(
    std::shared_ptr<Type> type,
    types::TypeQualifiers qualifiers
) : type(type), qualifiers(qualifiers) {}

std::string QualifiedType::codegenName() const {
    std::string res {};
    res += type ? type->codegenName() : "auto";
    if (qualifiers.isConst) {
        res += " const";
    }
    return res;
}

std::string QualifiedType::toString() const {
    std::string res {};
    res += type ? type->toString() : "auto";
    if (qualifiers.isConst) {
        res += " const";
    }
    return res;
}

bool QualifiedType::convertibleTo(QualifiedType const& other) const {
    if (!type) return false;
    return type->convertibleTo(other.type);
}

bool QualifiedType::castableTo(QualifiedType const& other) const {
    if (qualifiers.isConst && !other.qualifiers.isConst) {
        return false;
    }
    return type->castableTo(other.type);
}


FunctionType::FunctionType(
    Compiler& compiler,
    QualifiedType const& returnType,
    std::vector<QualifiedType> const& parameters
) : Type(compiler, types::DataType::Function),
    m_returnType(returnType), m_parameters(parameters) {}

QualifiedType const& FunctionType::getReturnType() {
    return m_returnType;
}

void FunctionType::setReturnType(QualifiedType const& type) {
    m_returnType = type;
}

std::vector<QualifiedType> const& FunctionType::getParameters() {
    return m_parameters;
}

std::string FunctionType::codegenName() const {
    auto res = m_returnType.codegenName() + "(*)(";
    bool first = true;
    for (auto& param : m_parameters) {
        if (!first) {
            res += ", ";
        }
        first = false;
        res += param.codegenName();
    }
    res += ")";
    return res;
}

std::string FunctionType::toString() const {
    std::string res = "fun (";
    bool first = true;
    for (auto& param : m_parameters) {
        if (!first) {
            res += ", ";
        }
        first = false;
        res += param.codegenName();
    }
    res += ") -> " + m_returnType.codegenName();
    return res;
}


ArrayType::ArrayType(Compiler& compiler, QualifiedType const& inner, size_t size) 
 : Type(compiler, types::DataType::Array),
   m_inner(inner), m_size(size) {}

QualifiedType const& ArrayType::getInnerType() {
    return m_inner;
}

std::string ArrayType::codegenName() const {
    if (m_size) {
        return "std::array<" + m_inner.codegenName() + ", " + std::to_string(m_size) + ">";
    } else {
        return "std::vector<" + m_inner.codegenName() + ">";
    }
}

std::string ArrayType::toString() const {
    if (m_size) {
        return m_inner.codegenName() + "[" + std::to_string(m_size) + "]";
    } else {
        return m_inner.codegenName() + "[]";
    }
}


ClassType::ClassType(Compiler& compiler, std::string const& name)
 : Type(compiler, types::DataType::Class),
   m_name(name) {}

std::string const& ClassType::getName() const {
    return m_name;
}

std::unordered_map<std::string, std::shared_ptr<Type>> const& ClassType::getMembers() const {
    return m_members;
}

std::string ClassType::codegenName() const {
    return m_name;
}

std::string ClassType::toString() const {
    return m_name;
}


PointerType::PointerType(
    Compiler& compiler,
    QualifiedType const& inner,
    types::PointerType pointerType
) : Type(compiler, types::DataType::Pointer),
    m_inner(inner),
    m_pointerType(pointerType) {}

QualifiedType const& PointerType::getInnerType() {
    return m_inner;
}

std::string PointerType::codegenName() const {
    std::string res = m_inner.codegenName();
    switch (m_pointerType) {
        case types::PointerType::Pointer:   return res + "*";
        case types::PointerType::Reference: return res + "&";
        case types::PointerType::Move:      return res + "&&";
        default: return "something_has_gone_terribly_wrong_in_the_gdml_compiler";
    }
}

std::string PointerType::toString() const {
    // same thing
    return codegenName();
}
