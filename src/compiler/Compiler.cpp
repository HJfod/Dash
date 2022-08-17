#include "Compiler.hpp"
#include "GDML.hpp"
#include <fstream>
#include <parser/AST.hpp>
#include "Instance.hpp"
#include <ranges>
#include "Value.hpp"

using namespace gdml;
using namespace gdml::io;


Scope::Scope(Compiler& compiler, bool blocking)
 : m_compiler(compiler), m_blocking(blocking)
{}

Compiler& Scope::getCompiler() {
    return m_compiler;
}

void Scope::pushType(std::string const& name, std::shared_ptr<Type> type) {
    m_types.insert({ m_compiler.getNameSpace() + name, type });
}

bool Scope::hasType(std::string const& name) const {
    return m_types.count(name);
}

Variable* Scope::pushVariable(std::string const& name, Variable const& var) {
    auto fullName = m_compiler.getNameSpace() + name;
    m_variables.insert({ fullName, var });
    return &m_variables.at(fullName);
}

bool Scope::hasVariable(std::string const& name) const {
    return m_variables.count(name);
}

Variable* Scope::getVariable(std::string const& name) {
    if (!m_variables.count(name)) {
        return nullptr;
    }
    return &m_variables.at(name);
}

FunctionEntity* Scope::pushFunction(std::string const& name, FunctionEntity const& fun) {
    auto fullName = m_compiler.getNameSpace() + name;
    if (m_functions.count(fullName)) {
        m_functions.at(fullName).push_back(fun);
    } else {
        m_functions.insert({ fullName, { fun }});
    }
    return &m_functions.at(fullName).back();
}

Scope::Search Scope::hasFunction(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    auto fullName = m_compiler.getNameSpace() + name;
    if (!m_functions.count(fullName)) {
        return Search::NotFound;
    }
    for (auto& fun : m_functions.at(fullName)) {
        if (
            !parameters.has_value() ||
            fun.type.type->matchParameters(parameters.value())
        ) {
            return Search::Found;
        }
    }
    return Search::NoMatchingOverload;
}

FunctionEntity* Scope::getFunction(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) {
    if (!m_functions.count(name)) {
        return nullptr;
    }
    for (auto& fun : m_functions.at(name)) {
        if (
            !parameters.has_value() ||
            fun.type.type->matchParameters(parameters.value())
        ) {
            return &fun;
        }
    }
    return nullptr;
}


Error Compiler::compile() {
    auto tres = m_ast->compile(m_instance);
    if (!tres) {
        auto err = tres.unwrapErr();
        m_instance.getShared().logError(err);
        return err.code;
    }
    return Error::OK;
}

std::vector<std::string> const& Compiler::getNameSpaceStack() const {
    return m_namespace;
}

std::string Compiler::getNameSpace() const {
    std::string res {};
    for (auto& ns : m_namespace) {
        res += ns + "::";
    }
    return res;
}

void Compiler::pushNameSpace(std::string const& name) {
    m_namespace.push_back(name);
}

void Compiler::popNameSpace(std::string const& name) {
    if (m_namespace.back() == name) {
        m_namespace.pop_back();
    } else {
        std::string stack = "";
        for (auto const& s : m_namespace) {
            stack += s + "::";
        }
        stack.erase(stack.end() - 2, stack.end());
        m_instance.getShared().logError({
            Error::InternalError,
            "Attempted to pop \"" + name + "\" off the top of "
            "the namespace stack, but it wasn't there. This is "
            "likely a bug within the compiler itself.",
            "",
            "Current stack: " + stack,
            Position { 0, 0 },
            Position { 0, 0 },
            m_instance.getSource()
        });
    }
}

void Compiler::pushScope(bool blocking) {
    m_scope.push_back(Scope(*this, blocking));
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


Entity* Compiler::getEntity(std::string const& name) {
    auto var = getVariable(name);
    if (var) return var;
    return getFunction(name, None);
}

bool Compiler::variableExists(std::string const& name) const {
    // prioritize innermost scope
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        // backwards test all current namespaces
        auto testName = name;
        if (scope.hasVariable(name)) {
            return true;
        }
        for (auto& ns : std::ranges::reverse_view(m_namespace)) {
            testName = ns + "::" + name;
            if (scope.hasVariable(name)) {
                return true;
            }
        }
    }
    return false;
}

Variable* Compiler::getVariable(std::string const& name) {
    // prioritize innermost scope
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        // backwards test all current namespaces
        auto testName = name;
        auto var = scope.getVariable(testName);
        if (var) {
            return var;
        }
        for (auto& ns : std::ranges::reverse_view(m_namespace)) {
            testName = ns + "::" + name;
            var = scope.getVariable(testName);
            if (var) {
                return var;
            }
        }
    }
    return nullptr;
}

bool Compiler::functionExists(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    // prioritize innermost scope
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        // backwards test all current namespaces
        auto testName = name;
        if (scope.hasFunction(name, parameters) == Scope::Search::Found) {
            return true;
        }
        for (auto& ns : std::ranges::reverse_view(m_namespace)) {
            testName = ns + "::" + name;
            if (scope.hasFunction(name, parameters) == Scope::Search::Found) {
                return true;
            }
        }
    }
    return false;
}

FunctionEntity* Compiler::getFunction(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) {
    // prioritize innermost scope
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        // backwards test all current namespaces
        auto testName = name;
        auto fun = scope.getFunction(testName, parameters);
        if (fun) {
            return fun;
        }
        for (auto& ns : std::ranges::reverse_view(m_namespace)) {
            testName = ns + "::" + name;
            fun = scope.getFunction(testName, parameters);
            if (fun) {
                return fun;
            }
        }
    }
    return nullptr;
}


bool Compiler::typeExists(std::string const& name) const {
    return getType(name).get();
}

std::shared_ptr<Type> Compiler::getType(std::string const& name) const {
    // prioritize innermost scope
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        // backwards test all current namespaces
        auto testName = name;
        if (scope.m_types.count(testName)) {
            return scope.m_types.at(testName);
        }
        for (auto& ns : std::ranges::reverse_view(m_namespace)) {
            testName = ns + "::" + name;
            if (scope.m_types.count(testName)) {
                return scope.m_types.at(testName);
            }
        }
    }
    return nullptr;
}

std::shared_ptr<Type> Compiler::getBuiltInType(types::DataType type) const {
    return getType(types::dataTypeToString(type));
}

void Compiler::codegen(std::ostream& stream) const noexcept {
    m_ast->codegen(m_instance, stream);
}

Compiler::Compiler(Instance& shared, ast::AST* ast)
 : m_instance(shared), m_ast(ast),
   m_formatter(*this), m_scope({ Scope(*this, true) }) {
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

void Formatter::newline(std::ostream& stream) const {
    if (m_compiler.getInstance().getShared().getFlag(Flags::PrettifyOutput)) {
        stream << "\n" << std::string(m_indentation, ' ');
    }
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
