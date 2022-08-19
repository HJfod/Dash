#include "Entity.hpp"
#include <parser/AST.hpp>
#include <ranges>

using namespace gdml;

Entity::Entity(
    std::shared_ptr<Namespace> container,
    std::string const& name,
    EntityType type
) : m_namespace(container), m_name(name), m_type(type) {}

EntityType Entity::getType() const {
    return m_type;
}

bool Entity::hasParentNamespace() const {
    return m_namespace != nullptr;
}

std::string Entity::getFullName() const {
    auto name = m_name.size() ? m_name : "`anonymous`";
    if (m_namespace) {
        if (m_namespace->isGlobal()) {
            return "::" + name;
        }
        if (m_namespace->hasParentNamespace()) {
            return m_namespace->getFullName() + "::" + name;
        }
    }
    return name;
}

TypeEntity::TypeEntity(
    std::shared_ptr<Namespace> container,
    std::string const& name,
    std::shared_ptr<Type> type
) : Entity(container, name, EntityType::Type), type(type) {}

ValueEntity::ValueEntity(
    std::shared_ptr<Namespace> container, 
    std::string const& name,
    EntityType type
) : Entity(container, name, type) {}

Variable::Variable(
    std::shared_ptr<Namespace> container,
    std::string const& name,
    QualifiedType const& type,
    Value* value,
    ast::VariableDeclExpr* decl
) : ValueEntity(container, name, EntityType::Variable),
    type(type), value(value), declaration(decl) {}

FunctionEntity::FunctionEntity(
    std::shared_ptr<Namespace> container,
    std::string const& name,
    QualifiedFunType const& type,
    ast::FunctionDeclStmt* decl
) : ValueEntity(container, name, EntityType::Function),
    type(type), declaration(decl) {}

Value* FunctionEntity::eval(Instance& instance) {
    if (!declaration->body.has_value()) {
        return nullptr;
    }
    return declaration->body.value()->eval(instance);
}

Namespace::Namespace(
    std::shared_ptr<Namespace> container,
    std::string const& name,
    bool isGlobal
) : Entity(container, name, EntityType::Namespace),
    m_isGlobal(isGlobal) {}

bool Namespace::isGlobal() const {
    return m_isGlobal;
}

void Namespace::pushEntity(
    std::string const& name,
    std::shared_ptr<Entity> entity
) {
    // does the provided name contain scope information?
    if (name.find("::") != std::string::npos) {
        auto ns = name.substr(0, name.find("::"));
        auto descopedName = name.substr(name.find("::") + 2);

        // does provided namespace exist?
        if (m_entities.count(ns)) {
            for (auto& ent : m_entities.at(ns)) {
                // todo: classes
                if (ent->getType() != EntityType::Namespace) {
                    continue;
                }
                return std::static_pointer_cast<Namespace>(ent)->pushEntity(
                    descopedName, entity
                );
            }
        }
        // make it then
        return makeEntity<Namespace>(ns)->pushEntity(descopedName, entity);
    }

    if (m_entities.count(name)) {
        m_entities.at(name).push_back(entity);
    } else {
        m_entities.insert({ name, { entity }});
    }
}

bool Namespace::hasEntity(
    std::string const& name,
    NamespaceParts const& currentNamespace,
    std::vector<NamespaceParts> const& testNamespaces,
    Option<EntityType> const& type,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    return getEntity(
        name, currentNamespace, testNamespaces, type, parameters
    ) != nullptr;
}

std::shared_ptr<const Namespace> Namespace::getNamespace(
    std::string const& name
) const {
    // does provided namespace exist?
    if (!m_entities.count(name)) {
        return nullptr;
    }
    // find entity that is a namespace
    for (auto& ent : m_entities.at(name)) {
        // todo: classes
        if (ent->getType() == EntityType::Namespace) {
            return std::static_pointer_cast<Namespace>(ent);
        }
    }
    return nullptr;
}

std::shared_ptr<const Namespace> Namespace::getNamespace(
    NamespaceParts const& name
) const {
    auto descoped = shared_from_this();
    for (auto& ns : name) {
        descoped = descoped->getNamespace(ns);
        if (!descoped) break;
    }
    return descoped;
}

std::shared_ptr<Entity> Namespace::getEntity(
    std::string const& rawName,
    Option<EntityType> const& type,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    auto name = rawName;
    // remove explicit namespace identifier
    if (name.starts_with("::")) {
        name.erase(0, 2);
    }

    // does the provided name contain scope information?
    if (name.find("::") != std::string::npos) {
        auto ns = name.substr(0, name.find("::"));
        auto descopedName = name.substr(name.find("::") + 2);

        // does provided namespace exist?
        if (auto space = getNamespace(ns)) {
            return space->getEntity(
                descopedName, type, parameters
            );
        }
        return nullptr;
    }
    
    // are there any entities with this name?
    if (!m_entities.count(name)) {
        return nullptr;
    }
    // if no parameters to figure out overload were 
    // provided, then it's the first matching item for 
    // this name
    if (!parameters.has_value()) {
        if (type.has_value()) {
            for (auto& ent : m_entities.at(name)) {
                if (ent->getType() == type) {
                    return ent;
                }
            }
            return nullptr;
        }
        return m_entities.at(name).front();
    }
    // match overload
    for (auto& ent : m_entities.at(name)) {
        if (ent->getType() != EntityType::Function) {
            continue;
        }
        if (auto fun = std::static_pointer_cast<FunctionEntity>(ent)) {
            if (fun->type.type->matchParameters(parameters.value())) {
                return fun;
            }
        }
    }
    // none found then
    return nullptr;
}

std::shared_ptr<Entity> Namespace::getEntity(
    std::string const& name,
    NamespaceParts const& currentNamespace,
    std::vector<NamespaceParts> const& testNamespaces,
    Option<EntityType> const& type,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    // is this in current scope?
    if (auto e = getEntity(name, type, parameters)) {
        return e;
    }
    // if the name is a full path, then do no other 
    // namespace resolution
    if (name.starts_with("::")) {
        return nullptr;
    }

    // check current namespaces
    auto descoped = shared_from_this();
    for (auto& ns : currentNamespace) {
        // check if namespace exists
        if (descoped = descoped->getNamespace(ns)) {
            // look for entity in descoped namespace
            if (auto e = descoped->getEntity(name, type, parameters)) {
                return e;
            }
        }
    }
    
    // what about extra using namespaces?
    for (auto& test : testNamespaces) {
        if (auto space = getNamespace(test)) {
            if (auto e = space->getEntity(name, type, parameters)) {
                return e;
            }
        }
    }

    // none found
    return nullptr;
}
