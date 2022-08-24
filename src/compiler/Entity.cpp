#include "Entity.hpp"
#include <parser/AST.hpp>
#include <ranges>
#include <compiler/Compiler.hpp>
#include <compiler/Instance.hpp>

using namespace gdml;

Entity::Entity(
    Instance& instance,
    std::shared_ptr<Namespace> container,
    std::string const& name,
    EntityType type,
    bool isExtern
) : m_instance(instance),
    m_namespace(container),
    m_name(name),
    m_type(type),
    m_isExtern(isExtern) {}

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

std::string Entity::getName() const {
    return m_name;
}

bool Entity::isExtern() const {
    return m_isExtern;
}


TypeEntity::TypeEntity(
    Instance& instance,
    std::shared_ptr<Namespace> container,
    std::string const& name,
    std::shared_ptr<Type> type,
    bool isExtern
) : Entity(instance, container, name, EntityType::Type, isExtern), type(type) {}

ValueEntity::ValueEntity(
    Instance& instance,
    std::shared_ptr<Namespace> container, 
    std::string const& name,
    EntityType type,
    bool isExtern
) : Entity(instance, container, name, type, isExtern) {}

Variable::Variable(
    Instance& instance,
    std::shared_ptr<Namespace> container,
    std::string const& name,
    QualifiedType const& type,
    std::shared_ptr<Value> value,
    ast::VariableDeclExpr* decl,
    bool isExtern
) : ValueEntity(instance, container, name, EntityType::Variable, isExtern),
    type(type), value(value), declaration(decl) {}

FunctionEntity::FunctionEntity(
    Instance& instance,
    std::shared_ptr<Namespace> container,
    std::string const& name,
    QualifiedFunType const& type,
    ast::AFunctionDeclStmt* decl,
    bool isExtern
) : ValueEntity(instance, container, name, EntityType::Function, isExtern),
    type(type), declaration(decl) {}

std::shared_ptr<Value> FunctionEntity::eval(Instance& instance) {
    if (!declaration->body.has_value()) {
        return nullptr;
    }
    return declaration->body.value()->eval(instance);
}

Namespace::Namespace(
    Instance& instance,
    std::shared_ptr<Namespace> container,
    std::string const& name,
    bool isGlobal,
    bool isExtern
) : Entity(instance, container, name, EntityType::Namespace, isExtern),
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
        auto nsName = name.substr(0, name.find("::"));
        auto descopedName = name.substr(name.find("::") + 2);

        // does provided namespace already exist?
        if (auto ns = getNamespaceMut(nsName)) {
            return ns->pushEntity(descopedName, entity);
        }
        // make it then
        return makeEntity<Namespace>(nsName)->pushEntity(descopedName, entity);
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
    Option<std::vector<Parameter>> const& parameters
) const {
    return getEntity(
        name, currentNamespace, testNamespaces, type, parameters
    ) != nullptr;
}

std::shared_ptr<Namespace> Namespace::getNamespaceMut(std::string const& name) {
    // does provided namespace exist?
    if (!m_entities.count(name)) {
        return nullptr;
    }
    // find entity that is a namespace
    for (auto& ent : m_entities.at(name)) {
        if (
            ent->getType() == EntityType::Namespace ||
            ent->getType() == EntityType::Class
        ) {
            return std::static_pointer_cast<Namespace>(ent);
        }
    }
    return nullptr;
}

std::shared_ptr<const Namespace> Namespace::getNamespace(
    std::string const& name
) const {
    // does provided namespace exist?
    if (!m_entities.count(name)) {
        if (m_isExtern) {
            return std::static_pointer_cast<Namespace>(
                std::const_pointer_cast<Namespace>(
                    shared_from_this()
                )->makeExtern(name, EntityType::Namespace, None)
            );
        }
        return nullptr;
    }
    // find entity that is a namespace
    for (auto& ent : m_entities.at(name)) {
        if (
            ent->getType() == EntityType::Namespace ||
            ent->getType() == EntityType::Class
        ) {
            return std::static_pointer_cast<Namespace>(ent);
        }
    }
    if (m_isExtern) {
        return std::static_pointer_cast<Namespace>(
            std::const_pointer_cast<Namespace>(
                shared_from_this()
            )->makeExtern(name, EntityType::Namespace, None)
        );
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

std::vector<std::shared_ptr<Entity>> Namespace::getEntities(
    std::string const& rawName,
    Option<EntityType> const& type
) const {
    std::vector<std::shared_ptr<Entity>> res;

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
            auto e = space->getEntities(descopedName, type);
            res.insert(res.end(), e.begin(), e.end());
        }
        return res;
    }

    // are there any entities with this name?
    if (!m_entities.count(name)) {
        return res;
    }
    
    // get all of them that match the possibly 
    // provided type
    for (auto& ent : m_entities.at(name)) {
        if (!type.has_value() || ent->getType() == type) {
            res.push_back(ent);
        }
    }
    return res;
}

std::shared_ptr<Entity> Namespace::getEntity(
    std::string const& rawName,
    Option<EntityType> const& type,
    Option<std::vector<Parameter>> const& parameters
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
        if (m_isExtern) {
            return std::const_pointer_cast<Namespace>(
                shared_from_this()
            )->makeExtern(name, type, parameters);
        }
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
            if (m_isExtern) {
                return std::const_pointer_cast<Namespace>(
                    shared_from_this()
                )->makeExtern(name, type, parameters);
            }
            return nullptr;
        }
        return m_entities.at(name).front();
    }
    // match overload
    for (auto& ent : m_entities.at(name)) {
        switch (ent->getType()) {
            case EntityType::Function: {
                auto fun = std::static_pointer_cast<FunctionEntity>(ent);
                if (fun->type.type->matchParameters(parameters.value())) {
                    return fun;
                }
            } break;

            default: break;
        }
    }

    if (m_isExtern) {
        return std::const_pointer_cast<Namespace>(
            shared_from_this()
        )->makeExtern(name, type, parameters);
    }
    
    // none found then
    return nullptr;
}

std::vector<std::shared_ptr<Entity>> Namespace::getEntities(
    std::string const& name,
    NamespaceParts const& currentNamespace,
    std::vector<NamespaceParts> const& testNamespaces,
    Option<EntityType> const& type
) const {
    std::vector<std::shared_ptr<Entity>> entities;

    // is this in current scope?
    auto e = getEntities(name, type);
    entities.insert(entities.end(), e.begin(), e.end());

    // if the name is a full path, then do no other 
    // namespace resolution
    if (name.starts_with("::")) {
        return entities;
    }

    // check current namespaces
    auto descoped = shared_from_this();
    for (auto& ns : currentNamespace) {
        // check if namespace exists
        while (descoped = descoped->getNamespace(ns)) {
            // look for entity in descoped namespace
            auto e = descoped->getEntities(name, type);
            entities.insert(entities.end(), e.begin(), e.end());
        }
    }
    
    // what about extra using namespaces?
    for (auto& test : testNamespaces) {
        if (auto space = getNamespace(test)) {
            auto e = space->getEntities(name, type);
            entities.insert(entities.end(), e.begin(), e.end());
        }
    }

    // none found
    return entities;
}

std::shared_ptr<Entity> Namespace::getEntity(
    std::string const& name,
    NamespaceParts const& currentNamespace,
    std::vector<NamespaceParts> const& testNamespaces,
    Option<EntityType> const& type,
    Option<std::vector<Parameter>> const& parameters
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
        while (descoped = descoped->getNamespace(ns)) {
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

std::shared_ptr<Entity> Namespace::makeExtern(
    std::string const& name,
    Option<EntityType> const& type,
    Option<std::vector<Parameter>> const& parameters
) {
    if (parameters || (type && type.value() == EntityType::Function)) {
        return makeEntity<FunctionEntity>(
            name, QualifiedFunType(
                m_instance.getCompiler().makeType<FunctionType>(
                    QualifiedType(
                        m_instance.getCompiler().makeType<ExternType>()
                    ),
                    (parameters ?
                        parameters.value() :
                        std::vector<Parameter>()
                    )
                )
            ), nullptr, true
        );
    }
    if (!type) {
        return nullptr;
    }

    switch (type.value()) {
        case EntityType::Class: return makeEntity<Class>(
            name, m_instance.getCompiler().makeType<ClassType>(name), true
        );

        case EntityType::Variable: return makeEntity<Variable>(
            name, 
            QualifiedType(
                m_instance.getCompiler().makeType<ExternType>()
            ),
            nullptr, nullptr, true
        );

        case EntityType::Type: return makeEntity<TypeEntity>(
            name, m_instance.getCompiler().makeType<BuiltInType>(types::DataType::Void)
        );

        case EntityType::Namespace: return makeEntity<Namespace>(
            name, false, true
        );

        default: break;
    }
    return nullptr;
}


Class::Class(
    Instance& instance,
    std::shared_ptr<Namespace> container,
    std::string const& name,
    std::shared_ptr<ClassType> classType,
    bool isExtern
) : Namespace(instance, container, name, false, isExtern), m_classType(classType) {
    m_type = EntityType::Class;
}

void Class::applyTypeDefinition() {
    m_classType->m_entity = std::static_pointer_cast<Class>(shared_from_this());
}

std::shared_ptr<ClassType> Class::getClassType() const {
    return m_classType;
}

std::shared_ptr<PointerType> Class::getClassTypePointer() const {
    return m_classType->m_compiler.makeType<PointerType>(
        QualifiedType(m_classType)
    );
}

bool Class::hasMember(std::string const& name) const {
    if (m_isExtern) return true;
    return hasEntity(name, {}, {}, EntityType::Variable, None);
}

bool Class::hasMemberFunction(
    std::string const& name,
    Option<std::vector<Parameter>> const& parameters
) const {
    if (m_isExtern) return true;
    return hasEntity(name, {}, {}, EntityType::Function, parameters);
}

std::shared_ptr<Variable> Class::getMember(std::string const& name) const {
    return std::static_pointer_cast<Variable>(
        getEntity(name, {}, {}, EntityType::Variable, None)
    );
}

std::shared_ptr<FunctionEntity> Class::getMemberFunction(
    std::string const& name,
    Option<std::vector<Parameter>> const& parameters
) const {
    if (parameters) {
        std::vector<Parameter> paramsWithThis {
            { "this", QualifiedType(this->getClassTypePointer()) }
        };
        paramsWithThis.insert(
            paramsWithThis.end(),
            parameters.value().begin(),
            parameters.value().end()
        );
        if (auto memberFun = std::static_pointer_cast<FunctionEntity>(
            getEntity(name, {}, {}, EntityType::Function, paramsWithThis)
        )) {
            return memberFun;
        }
    }
    return std::static_pointer_cast<FunctionEntity>(
        getEntity(name, {}, {}, EntityType::Function, parameters)
    );
}

