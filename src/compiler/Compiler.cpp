#include "Compiler.hpp"
#include "GDML.hpp"
#include <fstream>
#include <parser/AST.hpp>
#include "Instance.hpp"
#include <ranges>
#include "Value.hpp"
#include "Entity.hpp"

using namespace gdml;
using namespace gdml::io;

Scope::Scope(bool isGlobal)
  : m_global(std::make_shared<Namespace>(nullptr, "", isGlobal)) {}

void Scope::useNamespace(NamespaceParts const& space) {
    m_namespaces.push_back(space);
}

void Scope::pushNamespace(std::string const& name) {
    m_currentNamespace.push_back(name);
}

void Scope::popNamespace() {
    m_currentNamespace.pop_back();
}

std::string Scope::currentNamespace() const {
    std::string res {};
    for (auto& ns : m_currentNamespace) {
        res += ns + "::";
    }
    return res;
}

bool Scope::hasEntity(
    std::string const& name,
    Option<EntityType> type,
    Option<std::vector<Parameter>> const& parameters
) const {
    return m_global->hasEntity(name, m_currentNamespace, m_namespaces, type, parameters);
}

std::shared_ptr<Entity> Scope::getEntity(
    std::string const& name,
    Option<EntityType> type,
    Option<std::vector<Parameter>> const& parameters
) const {
    return m_global->getEntity(name, m_currentNamespace, m_namespaces, type, parameters);
}

std::vector<std::shared_ptr<Entity>> Scope::getEntities(
    std::string const& name,
    Option<EntityType> type
) const {
    return m_global->getEntities(name, m_currentNamespace, m_namespaces, type);
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

void Compiler::pushScope() {
    m_scope.emplace_back(false);
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

bool Compiler::hasEntity(
    std::string const& name,
    Option<EntityType> type,
    Option<std::vector<Parameter>> const& parameters,
    bool checkAllScopes
) const {
    // if the name is a full path then 
    // search only global namespace
    if (name.starts_with("::")) {
        return m_scope.front().hasEntity(name, type, parameters);
    }
    // otherwise prioritize innermost namespace
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        if (scope.hasEntity(name, type, parameters)) {
            return true;
        }
        if (!checkAllScopes) break;
    }
    return false;
}

std::shared_ptr<Entity> Compiler::getEntity(
    std::string const& name,
    Option<EntityType> type,
    Option<std::vector<Parameter>> const& parameters
) const {
    // if the name is a full path then 
    // search only global namespace
    if (name.starts_with("::")) {
        return m_scope.front().getEntity(name, type, parameters);
    }
    // otherwise prioritize innermost namespace
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        if (auto r = scope.getEntity(name, type, parameters)) {
            return r;
        }
    }
    return nullptr;
}

std::vector<std::shared_ptr<Entity>> Compiler::getEntities(
    std::string const& name,
    Option<EntityType> type
) const {
    // if the name is a full path then 
    // search only global namespace
    if (name.starts_with("::")) {
        return m_scope.front().getEntities(name, type);
    }

    std::vector<std::shared_ptr<Entity>> entities;
    // otherwise prioritize innermost namespace
    for (auto& scope : std::ranges::reverse_view(m_scope)) {
        auto get = scope.getEntities(name, type);
        entities.insert(entities.end(), get.begin(), get.end());
    }
    return entities;
}

std::shared_ptr<BuiltInType> Compiler::getBuiltInType(types::DataType type) const {
    auto entity = getEntityAs<TypeEntity>(
        types::dataTypeToString(type), EntityType::Type, None
    );
    return std::static_pointer_cast<BuiltInType>(entity ? entity->type : nullptr);
}

void Compiler::codegen(std::ostream& stream) const noexcept {
    m_ast->codegen(m_instance, stream);
}

Compiler::Compiler(Instance& shared, ast::AST* ast)
  : m_instance(shared), m_ast(ast),
    m_formatter(*this), m_scope()
{
    m_scope.emplace_back(true);
    loadBuiltinTypes();
    loadConstValues();
}

Compiler::~Compiler() {}

Instance& Compiler::getInstance() const {
    return m_instance;
}

void Compiler::loadBuiltinTypes() {
    size_t i = 0;
    for (auto& type : types::DATATYPES) {
        m_scope.back().makeEntity<TypeEntity>(
            types::DATATYPE_STRS[i], makeType<BuiltInType>(type)
        );
        i++;
    }
}

void Compiler::loadConstValues() {
    m_constValues = {
        {
            ConstValue::True,
            makeValue<NumeralValue<types::Bool>>(true)
        },

        {
            ConstValue::False,
            makeValue<NumeralValue<types::Bool>>(false)
        },

        {
            ConstValue::EmptyString,
            makeValue<StringValue>("")
        },

        {
            ConstValue::Zero,
            makeValue<NumeralValue<types::I32>>(0)
        },

        {
            ConstValue::Null,
            makeValue<PointerValue>(nullptr)
        },
    };
}

std::shared_ptr<Value> Compiler::getConstValue(ConstValue value) const {
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
    std::shared_ptr<Value> value
) : Value(compiler), m_value(value) {}

std::shared_ptr<Value> PointerValue::copy() {
    return m_compiler.makeValue<PointerValue>(m_value);
}

std::shared_ptr<Value> PointerValue::getValue() const {
    return m_value;
}

void PointerValue::setValue(std::shared_ptr<Value> value) {
    m_value = value;
}
