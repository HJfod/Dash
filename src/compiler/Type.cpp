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

Type::Type(Compiler& compiler, const types::TypeClass type)
 : m_compiler(compiler), m_class(type) {}

const types::TypeClass Type::getTypeClass() const {
    return m_class;
}

// std::shared_ptr<Value> Type::instantiate() {
//     #define INSTANTIATE_TYPE(t) \
//         case types::DataType::t: return new BuiltInValue<types::t>(\
//             m_compiler, types::t()\
//         )
//     switch (m_type) {
//         default: return nullptr;
//         INSTANTIATE_TYPE(Bool);
//         INSTANTIATE_TYPE(I8);
//         INSTANTIATE_TYPE(I16);
//         INSTANTIATE_TYPE(I32);
//         INSTANTIATE_TYPE(I64);
//         INSTANTIATE_TYPE(U8);
//         INSTANTIATE_TYPE(U16);
//         INSTANTIATE_TYPE(U32);
//         INSTANTIATE_TYPE(U64);
//         INSTANTIATE_TYPE(F32);
//         INSTANTIATE_TYPE(F64);
//         INSTANTIATE_TYPE(Char);
//         INSTANTIATE_TYPE(String);
//     }
// }

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

bool BuiltInType::convertibleTo(std::shared_ptr<Type> other) const {
    if (other->getTypeClass() == types::TypeClass::BuiltIn) {
        return m_type == std::static_pointer_cast<BuiltInType>(other)->m_type;
    }
    return false;
}

bool BuiltInType::castableTo(std::shared_ptr<Type> other) const {
    if (Type::castableTo(other)) {
        return true;
    }
    return other->getTypeClass() == types::TypeClass::BuiltIn;
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


FunctionType::FunctionType(
    Compiler& compiler,
    QualifiedType const& returnType,
    std::vector<QualifiedType> const& parameters
) : Type(compiler, types::TypeClass::Function),
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

bool FunctionType::matchParameters(
    std::vector<QualifiedType> const& parameters
) const {
    size_t i = 0;
    for (auto& param : m_parameters) {
        if (!parameters.at(i).convertibleTo(param)) {
            return false;
        }
        i++;
    }
    return true;
}

bool FunctionType::convertibleTo(std::shared_ptr<Type> other) const {
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
) : Type(compiler, types::TypeClass::Pointer),
    m_inner(inner),
    m_pointerType(pointerType) {}

QualifiedType const& PointerType::getInnerType() {
    return m_inner;
}

bool PointerType::convertibleTo(std::shared_ptr<Type> other) const {
    return other->getTypeClass() == types::TypeClass::Pointer;
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
