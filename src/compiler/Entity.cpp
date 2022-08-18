#include "Entity.hpp"
#include <parser/AST.hpp>

using namespace gdml;

Entity::Entity(
    std::shared_ptr<Namespace> container,
    std::string const& name,
    EntityType type
) : m_namespace(container), m_name(name), m_type(type) {}

EntityType Entity::getType() const {
    return m_type;
}

std::string Entity::getFullName() const {
    if (m_namespace) {
        return m_namespace->getFullName() + "::" + m_name;
    }
    return m_name;
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
    std::string const& name
) : Entity(container, name, EntityType::Namespace) {
    std::cout << __FUNCTION__ << " -> " << this << "\n";
}

Namespace::~Namespace() {
    std::cout << __FUNCTION__ << " -> " << this << "\n";
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
    Option<EntityType> type,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    return getEntity(name, type, parameters) != nullptr;
}

std::shared_ptr<Entity> Namespace::getEntity(
    std::string const& name,
    Option<EntityType> type,
    Option<std::vector<QualifiedType>> const& parameters
) const {
    // does the provided name contain scope information?
    if (name.find("::") != std::string::npos) {
        auto ns = name.substr(0, name.find("::"));
        auto descopedName = name.substr(name.find("::") + 2);

        // does provided namespace exist?
        if (!m_entities.count(ns)) {
            return nullptr;
        }
        for (auto& ent : m_entities.at(ns)) {
            // todo: classes
            if (ent->getType() == EntityType::Namespace) {
                return std::static_pointer_cast<Namespace>(ent)->getEntity(
                    descopedName, type, parameters
                );
            }
        }
        return nullptr;
    }
    // are there any entities with this name?
    if (!m_entities.count(name)) {
        return nullptr;
    }
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
    // if no parameters to figure out overload were 
    // provided, then it's the first matching item for 
    // this name
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
    return nullptr;
}
