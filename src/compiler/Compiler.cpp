#include "Compiler.hpp"
#include "GDML.hpp"
#include <fstream>
#include <parser/AST.hpp>
#include "Instance.hpp"
#include <ranges>
#include "Value.hpp"

using namespace gdml;
using namespace gdml::io;

#define SCOPES_SEARCH_BY(fun, ...) \
    for (auto& scope : std::ranges::reverse_view(m_scope)) {\
        if (scope.fun(__VA_ARGS__)) {\
            return true;\
        }\
    }\
    return false

#define SCOPES_FIND_BY(fun, ...) \
    for (auto& scope : std::ranges::reverse_view(m_scope)) {\
        if (auto x = scope.fun(__VA_ARGS__)) {\
            return x;\
        }\
    }\
    return nullptr

Error Compiler::compile() {
    auto tres = m_ast->compile(m_instance);
    if (!tres) {
        auto err = tres.unwrapErr();
        m_instance.getShared().logError(err);
        return err.code;
    }
    return Error::OK;
}

void Compiler::pushScope() {
    m_scope.push_back(Scope(*this));
}

void Compiler::popScope() {
    m_scope.pop_back();
}

Scope& Compiler::getScope(size_t offset) {
    if (offset) {
        return *(m_scope.rbegin() + offset);
    }
    return m_scope.back();
}

bool Compiler::hasType(std::string const& name) const {
    SCOPES_SEARCH_BY(hasType, name);
}

std::shared_ptr<Type> Compiler::getType(std::string const& name) const {
    SCOPES_FIND_BY(getType, name);
}

bool Compiler::hasEntity(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) {
    SCOPES_SEARCH_BY(hasEntity, name, parameters);
}

std::shared_ptr<Entity> Compiler::getEntity(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) {
    SCOPES_FIND_BY(getEntity, name, parameters);
}

bool Compiler::hasVariable(std::string const& name) const {
    SCOPES_SEARCH_BY(hasVariable, name);
}

std::shared_ptr<Variable> Compiler::getVariable(std::string const& name) {
    SCOPES_FIND_BY(getVariable, name);
}

Scope::Search Compiler::hasFunction(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    Scope::Search status = Scope::Search::NotFound;
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        switch (scope.hasFunction(name, parameters)) {
            case Scope::Search::Found: {
                return Scope::Search::Found;
            } break;
            
            case Scope::Search::NoMatchingOverload: {
                status = Scope::Search::NoMatchingOverload;
            } break;

            default: break;
        }
    }
    return status;
}

std::shared_ptr<FunctionEntity> Compiler::getFunction(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) {
    SCOPES_FIND_BY(getFunction, name, parameters);
}

std::shared_ptr<Type> Compiler::getBuiltInType(types::DataType type) const {
    return getType(types::dataTypeToString(type));
}

void Compiler::codegen(std::ostream& stream) const noexcept {
    m_ast->codegen(m_instance, stream);
}

Compiler::Compiler(Instance& shared, ast::AST* ast)
 : m_instance(shared), m_ast(ast),
   m_formatter(*this), m_scope({ Scope(*this) }) {
    loadBuiltinTypes();
    loadConstValues();
}

Compiler::~Compiler() {
    for (auto& value : m_values) {
        delete value;
    }
}

Instance& Compiler::getInstance() const {
    return m_instance;
}

void Compiler::loadBuiltinTypes() {
    static std::array<types::DataType, 12> STATIC_CASTABLE {
        types::DataType::I8,  types::DataType::I16,
        types::DataType::I32, types::DataType::I64,
        types::DataType::U8,  types::DataType::U16,
        types::DataType::U32, types::DataType::U64,
        types::DataType::F32, types::DataType::F64,
        types::DataType::Bool,
        types::DataType::Char,
    };

    size_t i = 0;
    for (auto& type : types::DATATYPES) {
        m_scope.back().pushType(types::DATATYPE_STRS[i], makeType(type));
        i++;
    }
    for (auto& fromDataType : STATIC_CASTABLE) {
        auto fromType = getBuiltInType(fromDataType);

        for (auto& intoDataType : STATIC_CASTABLE) {
            auto intoType = getBuiltInType(intoDataType);

            fromType->addCastOperatorFor(
                intoType,
                "static_cast<" + intoType->codegenName() + ">"
            );
        }
    }
}

void Compiler::loadConstValues() {
    m_constValues = {
        {
            ConstValue::True,
            makeValue<BuiltInValue<types::Bool>>(true)
        },

        {
            ConstValue::False,
            makeValue<BuiltInValue<types::Bool>>(false)
        },

        {
            ConstValue::EmptyString,
            makeValue<BuiltInValue<types::String>>("")
        },

        {
            ConstValue::Zero,
            makeValue<BuiltInValue<types::I32>>(0)
        },

        {
            ConstValue::Null,
            makeValue<PointerValue>(nullptr)
        },
    };
}

Value* Compiler::getConstValue(ConstValue value) const {
    return m_constValues.at(value);
}


Formatter::Formatter(Compiler& compiler) : m_compiler(compiler) {}

Formatter& Compiler::getFormatter() {
    return m_formatter;
}

void Formatter::pushIndent() {
    m_indentation += 4;
}

void Formatter::popIndent() {
    m_indentation -= 4;
}

void Formatter::newLine(std::ostream& stream) const {
    if (m_compiler.getInstance().getShared().getFlag(Flags::PrettifyOutput)) {
        stream << "\n" << std::string(m_indentation, ' ');
    }
}

void Formatter::semiColon(std::ostream& stream) {
    if (m_skipSemiColon) {
        m_skipSemiColon = false;
    } else {
        stream << ";";
    }
}

void Formatter::skipSemiColon() {
    m_skipSemiColon = true;
}


PointerValue::PointerValue(
    Compiler& compiler,
    Value* value
) : Value(compiler), m_value(value) {}

Value* PointerValue::copy() {
    return m_compiler.makeValue<PointerValue>(m_value);
}

Value* PointerValue::getValue() const {
    return m_value;
}

void PointerValue::setValue(Value* value) {
    m_value = value;
}
