#include "Type.hpp"
#include "Compiler.hpp"
#include "Instance.hpp"
#include "GDML.hpp"
#include "Value.hpp"
#include <parser/AST.hpp>
#include <parser/Token.hpp>

#define DO_PRETTIFY \
    m_compiler.getInstance().getShared().getFlag(Flags::PrettifyOutput)

using namespace gdml;

Parameter::Parameter(QualifiedType const& type) : type(type) {}
Parameter::Parameter(std::string const& name, QualifiedType const& type)
  : name(name), type(type) {}
Parameter::Parameter(ast::VariableDeclExpr* decl)
  : name(decl->name), type(decl->evalType) {}

std::string Parameter::toString() const {
    return (name ? name.value() + ": " : "") + type.toString();
}

Type::Type(Compiler& compiler, const types::TypeClass type)
 : m_compiler(compiler), m_class(type) {}

const types::TypeClass Type::getTypeClass() const {
    return m_class;
}

bool Type::codegenConvert(
    std::shared_ptr<Type> other,
    ast::ValueExpr* target,
    std::ostream& stream
) const {
    // for most of these C++ is the exact same
    target->codegen(m_compiler.getInstance(), stream);
    return true;
}

bool Type::castableTo(std::shared_ptr<Type> other) const {
    // todo: global cast definitions
    return false;
}

bool Type::codegenCast(
    std::shared_ptr<Type> other,
    ast::ValueExpr* target,
    std::ostream& stream
) {
    return false;
}

ImplStatus Type::implementsUnaryOperator(TokenType op, bool prefix) const {
    return ImplStatus::None;
}

std::shared_ptr<Value> Type::evalUnary(TokenType op, std::shared_ptr<Value> target, bool prefix) {
    return nullptr;
}

bool Type::codegenUnary(
    ast::UnaryExpr const* expr,
    std::ostream& stream
) const {
    return false;
}

ImplStatus Type::implementsBinaryOperator(
    TokenType op,
    std::shared_ptr<Type> other
) const {
    return ImplStatus::None;
}

std::shared_ptr<Value> Type::evalBinary(TokenType op, std::shared_ptr<Value> first, std::shared_ptr<Value> second) {
    return nullptr;
}

bool Type::codegenBinary(
    ast::BinaryExpr const* expr,
    std::ostream& stream
) const {
    return false;
}

bool Type::implementsMemberOperator() const {
    return true;
}

QualifiedType Type::typeOfMember(
    std::string const& name,
    Option<std::vector<Parameter>> const& args
) const {
    return QualifiedType::NO_TYPE;
}

bool Type::hasDefinition() const {
    return true;
}



BuiltInType::BuiltInType(Compiler& compiler, const types::DataType type)
  : Type(compiler, types::TypeClass::BuiltIn), m_type(type) {}

const types::DataType BuiltInType::getType() const {
    return m_type;
}

std::string BuiltInType::codegenName() const {
    return types::dataTypeToCppType(m_type);
}

std::string BuiltInType::toString() const {
    return types::dataTypeToString(m_type);
}

bool BuiltInType::convertibleTo(std::shared_ptr<Type> other, bool strict) const {
    if (other && other->getTypeClass() == types::TypeClass::BuiltIn) {
        auto otherAsB = std::static_pointer_cast<BuiltInType>(other);
        return
            m_type == otherAsB->m_type ||
            (!strict && otherAsB->m_type == types::DataType::Bool);
    }
    return false;
}

bool BuiltInType::codegenConvert(
    std::shared_ptr<Type> other,
    ast::ValueExpr* target,
    std::ostream& stream
) const {
    // only special one is string to bool
    if (
        m_type == types::DataType::String && 
        std::static_pointer_cast<BuiltInType>(other)->m_type == types::DataType::Bool
    ) {
        // users can't add implicit conversions 
        // so we know this one's a bool
        stream << "(";
        target->codegen(m_compiler.getInstance(), stream);
        stream << ").size()";
        return true;
    }
    return Type::codegenConvert(other, target, stream);
}

bool BuiltInType::castableTo(std::shared_ptr<Type> other) const {
    if (Type::castableTo(other)) {
        return true;
    }
    return other->getTypeClass() == types::TypeClass::BuiltIn;
}

bool BuiltInType::codegenCast(
    std::shared_ptr<Type> other,
    ast::ValueExpr* target,
    std::ostream& stream
) {
    if (!Type::codegenCast(other, target, stream)) {
        stream << "static_cast<" << other->codegenName() << ">(";
        target->codegen(m_compiler.getInstance(), stream);
        stream << ")";
    }
    return true;
}

ImplStatus BuiltInType::implementsUnaryOperator(
    TokenType op, bool prefix
) const {
    auto userImpl = Type::implementsUnaryOperator(op, prefix);
    if (userImpl != ImplStatus::None) {
        return userImpl;
    }
    return m_type != types::DataType::String ?
        ImplStatus::Constexpr : ImplStatus::None;
}

std::shared_ptr<Value> BuiltInType::evalUnary(TokenType op, std::shared_ptr<Value> target, bool prefix) {
    if (auto userImpl = Type::evalUnary(op, target, prefix)) {
        return userImpl;
    }
    // only built-in implementations are those 
    // between other built-in values
    return std::static_pointer_cast<ABuiltInValue>(target)->applyUnary(op, prefix);
}

bool BuiltInType::codegenUnary(
    ast::UnaryExpr const* expr,
    std::ostream& stream
) const {
    if (Type::codegenUnary(expr, stream)) {
        return true;
    }
    if (expr->type == ast::UnaryExpr::Prefix) {
        stream << tokenTypeToString(expr->op);
    }
    expr->value->codegen(m_compiler.getInstance(), stream);
    if (expr->type == ast::UnaryExpr::Suffix) {
        stream << tokenTypeToString(expr->op);
    }
    return true;
}

ImplStatus BuiltInType::implementsBinaryOperator(
    TokenType op,
    std::shared_ptr<Type> other
) const {
    auto userImpl = Type::implementsBinaryOperator(op, other);
    if (userImpl != ImplStatus::None) {
        return userImpl;
    }
    // only built-in operators remain
    if (other->getTypeClass() != types::TypeClass::BuiltIn) {
        return ImplStatus::None;
    }
    auto otherType = std::static_pointer_cast<BuiltInType>(other)->m_type;
    // string is special wecial
    if (m_type == types::DataType::String) {
        if (types::dataTypeIsInteger(otherType)) {
            return op == TokenType::Mul ? ImplStatus::Constexpr : ImplStatus::None;
        }
        if (otherType != types::DataType::String) {
            return ImplStatus::None;
        }
        return
            (op == TokenType::Assign ||
            op == TokenType::Add ||
            op == TokenType::AddAssign ||
            op == TokenType::Equal ||
            op == TokenType::NotEqual ||
            op == TokenType::Dot) ?
                ImplStatus::Constexpr :
                ImplStatus::None;
    }
    // ok then, must be an integral or floating type
    return m_type == otherType ? ImplStatus::Constexpr : ImplStatus::None;
}

std::shared_ptr<Value> BuiltInType::evalBinary(TokenType op, std::shared_ptr<Value> first, std::shared_ptr<Value> second) {
    if (auto userImpl = Type::evalBinary(op, first, second)) {
        return userImpl;
    }
    // only built-in implementations are those 
    // between other built-in values
    return std::static_pointer_cast<ABuiltInValue>(first)->applyBinary(
        op, std::static_pointer_cast<ABuiltInValue>(second)
    );
}

bool BuiltInType::codegenBinary(
    ast::BinaryExpr const* expr,
    std::ostream& stream
) const {
    if (Type::codegenBinary(expr, stream)) {
        return true;
    }

    switch (expr->op) {
        // these have been chopped into different AST stuff
        case TokenType::DoubleQuestion:
        case TokenType::QuestionAssign: {
            stream <<
                "static_assert(false, \""
                "Something has gone wrong in the GDML compiler. "
                "You should not see this."
                "\");";
        } break;

        case TokenType::Pow: {
            stream << "static_cast<" << codegenName() << ">(";
            stream << "pow(";

            expr->LHS->codegen(m_compiler.getInstance(), stream);
            stream << ",";
            if (DO_PRETTIFY) stream << " ";
            expr->RHS->codegen(m_compiler.getInstance(), stream);

            stream << "))";
        } break;

        default: {
            expr->LHS->codegen(m_compiler.getInstance(), stream);
            if (DO_PRETTIFY) stream << " ";
            stream << tokenTypeToString(expr->op);
            if (DO_PRETTIFY) stream << " ";
            expr->RHS->codegen(m_compiler.getInstance(), stream);
        } break;
    }

    return true;
}

bool BuiltInType::implementsMemberOperator() const {
    return m_type != types::DataType::Void;
}


FunctionType::FunctionType(
    Compiler& compiler,
    QualifiedType const& returnType,
    std::vector<Parameter> const& parameters,
    FunType funType
) : Type(compiler, types::TypeClass::Function),
    m_returnType(returnType), m_parameters(parameters),
    m_funType(funType) {}

FunctionType::FunType FunctionType::getFunType() const {
    return m_funType;
}

QualifiedType const& FunctionType::getReturnType() {
    return m_returnType;
}

void FunctionType::setReturnType(QualifiedType const& type) {
    m_returnType = type;
}

std::vector<Parameter>& FunctionType::getParameters() {
    return m_parameters;
}

void FunctionType::insertParameter(size_t index, Parameter const& parameter) {
    m_parameters.insert(m_parameters.begin() + index, parameter);
}

bool FunctionType::matchParameters(
    std::vector<Parameter> const& parameters
) const {
    if (parameters.size() != m_parameters.size()) {
        return false;
    }
    size_t i = 0;
    for (auto& param : parameters) {
        // default to positional matching
        auto toMatch = m_parameters.at(i);
        
        // if named, match by name
        if (param.name) {
            bool found = false;
            for (auto& arg : m_parameters) {
                if (arg.name && arg.name.value() == param.name) {
                    found = true;
                    toMatch = arg;
                }
            }
            if (!found) {
                return false;
            }
        }
        if (!toMatch.type.convertibleTo(param.type)) {
            return false;
        }
        i++;
    }
    return true;
}

bool FunctionType::convertibleTo(std::shared_ptr<Type> other, bool strict) const {
    return false;
}

bool FunctionType::implementsMemberOperator() const {
    return false;
}

std::string FunctionType::codegenName() const {
    auto res = m_returnType.codegenName() + "(*)(";
    bool first = true;
    for (auto& param : m_parameters) {
        if (!first) {
            res += ", ";
        }
        first = false;
        res += param.type.codegenName();
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
        res += param.toString();
    }
    res += ") -> " + m_returnType.toString();
    return res;
}


ArrayType::ArrayType(
    Compiler& compiler, QualifiedType const& inner, size_t size
) : Type(compiler, types::TypeClass::Array),
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


ClassType::ClassType(
    Compiler& compiler, std::string const& name
) : Type(compiler, types::TypeClass::Class),
    m_name(name) {}

std::string const& ClassType::getName() const {
    return m_name;
}

bool ClassType::convertibleTo(std::shared_ptr<Type> other, bool strict) const {
    return false;
}

std::shared_ptr<Class> ClassType::getEntity() {
    return m_entity;
}

QualifiedType ClassType::typeOfMember(
    std::string const& name,
    Option<std::vector<Parameter>> const& args
) const {
    if (!m_entity) {
        return QualifiedType::NO_TYPE;
    }
    if (args) {
        auto function = m_entity->getMemberFunction(name, args);
        return function ? function->getValueType() : QualifiedType::NO_TYPE;
    }
    auto member = m_entity->getMember(name);
    return member ? member->getValueType() : QualifiedType::NO_TYPE;
}

bool ClassType::hasDefinition() const {
    return m_entity.get();
}

std::string ClassType::codegenName() const {
    return m_name;
}

std::string ClassType::toString() const {
    return m_name;
}


PointerType::PointerType(
    Compiler& compiler,
    QualifiedType const& inner
) : Type(compiler, types::TypeClass::Pointer),
    m_inner(inner) {}

QualifiedType const& PointerType::getInnerType() {
    return m_inner;
}

bool PointerType::convertibleTo(std::shared_ptr<Type> other, bool strict) const {
    return
        (other->getTypeClass() == types::TypeClass::Pointer) ||
        (
            other->getTypeClass() == types::TypeClass::BuiltIn && 
            std::static_pointer_cast<BuiltInType>(other)->getType() == types::DataType::Bool
        );
}

bool PointerType::implementsMemberOperator() const {
    return false;
}

std::string PointerType::codegenName() const {
    return m_inner.codegenName() + "*";
}

std::string PointerType::toString() const {
    // same thing
    return codegenName();
}


ExternType::ExternType(
    Compiler& compiler
) : Type(compiler, types::TypeClass::Extern) {}

bool ExternType::convertibleTo(std::shared_ptr<Type> other, bool strict) const {
    return true;
}

bool ExternType::implementsMemberOperator() const {
    return true;
}

std::string ExternType::codegenName() const {
    return "extern";
}

std::string ExternType::toString() const {
    return "extern";
}


bool gdml::typeIsVoid(std::shared_ptr<Type> type) {
    return
        type.get() && (
            type->getTypeClass() == types::TypeClass::BuiltIn &&
            std::static_pointer_cast<BuiltInType>(type)->getType() == types::DataType::Void
        );
}
