#include "Compiler.hpp"
#include "GDML.hpp"
#include <fstream>
#include <parser/AST.hpp>
#include "Instance.hpp"
#include <ranges>
#include "Value.hpp"

using namespace gdml;

Scope::Scope(Compiler& compiler) : m_compiler(compiler) {}

Compiler& Scope::getCompiler() {
    return m_compiler;
}

#define SCOPE_SEARCH_BY(member, name) \
    if (member.count(name)) {\
        return true;\
    }\
    auto testName = name;\
    for (auto& ns : std::ranges::reverse_view(m_namespace)) {\
        testName = ns + "::" + testName;\
        if (member.count(testName)) {\
            return true;\
        }\
    }\
    return false

#define SCOPE_FIND_BY(member, name) \
    if (member.count(name)) {\
        return member.at(name);\
    }\
    auto testName = name;\
    for (auto& ns : std::ranges::reverse_view(m_namespace)) {\
        testName = ns + "::" + testName;\
        if (member.count(testName)) {\
            return member.at(testName);\
        }\
    }\
    return nullptr

// types

void Scope::pushType(std::string const& name, std::shared_ptr<Type> type) {
    m_types.insert({ getNameSpace() + name, type });
}

bool Scope::hasType(std::string const& name) const {
    SCOPE_SEARCH_BY(m_types, name);
}

std::shared_ptr<Type> Scope::getType(std::string const& name) const {
    SCOPE_FIND_BY(m_types, name);
}

// entities

bool Scope::hasEntity(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) {
    if (hasVariable(name)) {
        return true;
    }
    return hasFunction(name, parameters) == Search::Found;
}

std::shared_ptr<Entity> Scope::getEntity(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) {
    if (auto entity = getVariable(name)) {
        return entity;
    }
    return getFunction(name, parameters);
}

// variables

std::shared_ptr<Variable> Scope::pushVariable(
    std::string const& name,
    std::shared_ptr<Variable> var
) {
    auto fullName = getNameSpace() + name;
    var->fullName = fullName;
    m_variables.insert({ fullName, var });
    return var;
}

bool Scope::hasVariable(std::string const& name) const {
    SCOPE_SEARCH_BY(m_variables, name);
}

std::shared_ptr<Variable> Scope::getVariable(std::string const& name) {
    SCOPE_FIND_BY(m_variables, name);
}

// functions

std::shared_ptr<FunctionEntity> Scope::pushFunction(
    std::string const& name,
    std::shared_ptr<FunctionEntity> fun
) {
    auto fullName = getNameSpace() + name;
    fun->fullName = fullName;
    if (m_functions.count(fullName)) {
        m_functions.at(fullName).push_back(fun);
    } else {
        m_functions.insert({ fullName, { fun }});
    }
    return fun;
}

Scope::Search Scope::hasFunction(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    auto status = Search::NotFound;
    auto testName = name;
    if (m_functions.count(testName)) {
        for (auto& fun : m_functions.at(testName)) {
            if (
                !parameters.has_value() ||
                fun->type.type->matchParameters(parameters.value())
            ) {
                return Search::Found;
            }
        }
        status = Search::NoMatchingOverload;
    }
    for (auto& ns : std::ranges::reverse_view(m_namespace)) {
        testName = ns + "::" + testName;
        if (m_functions.count(testName)) {
            for (auto& fun : m_functions.at(testName)) {
                if (
                    !parameters.has_value() ||
                    fun->type.type->matchParameters(parameters.value())
                ) {
                    return Search::Found;
                }
            }
            status = Search::NoMatchingOverload;
        }
    }
    return status;
}

std::shared_ptr<FunctionEntity> Scope::getFunction(
    std::string const& name,
    Option<std::vector<QualifiedType>> const& parameters
) {
    auto status = Search::NotFound;
    auto testName = name;
    if (m_functions.count(testName)) {
        for (auto& fun : m_functions.at(testName)) {
            if (
                !parameters.has_value() ||
                fun->type.type->matchParameters(parameters.value())
            ) {
                return fun;
            }
        }
        status = Search::NoMatchingOverload;
    }
    for (auto& ns : std::ranges::reverse_view(m_namespace)) {
        testName = ns + "::" + testName;
        if (m_functions.count(testName)) {
            for (auto& fun : m_functions.at(testName)) {
                if (
                    !parameters.has_value() ||
                    fun->type.type->matchParameters(parameters.value())
                ) {
                    return fun;
                }
            }
            status = Search::NoMatchingOverload;
        }
    }
    return nullptr;
}

// namespaces

void Scope::pushNameSpace(std::string const& name) {
    m_namespace.push_back(name);
}

void Scope::popNameSpace() {
    m_namespace.pop_back();
}

std::string Scope::getNameSpace() const {
    std::string res {};
    for (auto& ns : m_namespace) {
        res += ns + "::";
    }
    return res;
}


