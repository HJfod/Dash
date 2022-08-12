#include "Compiler.hpp"
#include "GDML.hpp"
#include <fstream>
#include <parser/AST.hpp>

using namespace gdml;
using namespace gdml::io;

std::string Type::getCodegenName() const {
    return m_cppEquivalent;
}

Error Compiler::compile() {
    auto tres = m_ast->compile(*this);
    if (!tres) {
        auto err = tres.unwrapErr();
        m_shared.logError(err);
        return err.code;
    }
    return Error::OK;
}

void Compiler::loadBuiltinTypes() {
    size_t i = 0;
    for (auto& type : types::NUMTYPES) {
        makeType(
            types::NUMTYPE_STRS[i],
            types::numberTypeToCppType(type)
        );
        i++;
    }
    makeType("string", "gd::string");
    makeType("void");
    makeType("bool");
    makeType("size_t");
}

Type* Compiler::makeType(
    std::string const& name,
    std::string const& cppEquivalent
) {
    auto type = new Type(name, cppEquivalent);
    m_types.insert({ name, type });
    return type;
}

Type* Compiler::makeType(std::string const& name) {
    return makeType(name, name);
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
        m_shared.logError({
            Error::InternalError,
            "Attempted to pop \"" + name + "\" off the top of "
            "the scope stack, but it wasn't there. This is "
            "likely a bug within the compiler itself.",
            "",
            "Current scope: " + stack,
            Position { 0, 0 },
            Position { 0, 0 },
            m_shared.getInputFile()
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
    m_ast->codegen(m_shared, stream);
}

Compiler::Compiler(GDML& shared, ast::AST* ast)
 : m_shared(shared), m_ast(ast) {
    loadBuiltinTypes();
}
