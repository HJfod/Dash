#include "Compiler.hpp"
#include "GDML.hpp"
#include <fstream>
#include <parser/AST.hpp>
#include "Instance.hpp"

using namespace gdml;
using namespace gdml::io;

Type::Type(Compiler& compiler, const types::DataType type)
 : m_compiler(compiler), m_type(type) {}

const types::DataType Type::getType() const {
    return m_type;
}

Value* Type::instantiate(TypeQualifiers const& qualifiers) {
    #define INSTANTIATE_TYPE(t) \
        case types::DataType::t: return new BuiltInValue<types::t>(types::t(), qualifiers)

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

std::string Type::codegen() const {
    return types::dataTypeToCppType(m_type);
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

void Compiler::loadBuiltinTypes() {
    size_t i = 0;
    for (auto& type : types::DATATYPES) {
        makeType(
            types::DATATYPE_STRS[i],
            types::dataTypeToCppType(type)
        );
        i++;
    }
}

std::vector<std::string> const& Compiler::getScope() const {
    return m_scope;
}

void Compiler::pushScope(std::string const& name) {
    m_scope.push_back(name);
}

void Compiler::popScope(std::string const& name) {
    if (m_scope.back() == name) {
        m_scope.pop_back();
    } else {
        std::string stack = "";
        for (auto const& s : m_scope) {
            stack += s + "::";
        }
        stack.erase(stack.end() - 2, stack.end());
        m_instance.getShared().logError({
            Error::InternalError,
            "Attempted to pop \"" + name + "\" off the top of "
            "the scope stack, but it wasn't there. This is "
            "likely a bug within the compiler itself.",
            "",
            "Current scope: " + stack,
            Position { 0, 0 },
            Position { 0, 0 },
            m_instance.getSource()
        });
    }
}

bool Compiler::typeExists(std::string const& name) const {
    if (m_types.count(name)) {
        return true;
    }
    std::string testScope = "";
    for (auto& scope : m_scope) {
        if (m_types.count(testScope + name)) {
            return true;
        }
        testScope += scope + "::";
    }
    return false;
}

Type* Compiler::getType(std::string const& name) const {
    if (!m_types.count(name)) {
        return nullptr;
    }
    return m_types.at(name);
}

void Compiler::codegen(std::ostream& stream) const noexcept {
    m_ast->codegen(m_instance, stream);
}

Compiler::Compiler(Instance& shared, ast::AST* ast)
 : m_instance(shared), m_ast(ast), m_formatter(*this) {
    loadBuiltinTypes();
}

Instance& Compiler::getInstance() const {
    return m_instance;
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
