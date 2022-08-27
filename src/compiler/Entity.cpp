#include "Entity.hpp"
#include <parser/AST.hpp>
#include <ranges>
#include <compiler/Compiler.hpp>
#include <compiler/Instance.hpp>
#include <compiler/GDML.hpp>

using namespace gdml;
using namespace std::string_literals;

#define PROPAGATE_FIND_ERR(val) \
    if (!val) {\
        return FindError {\
            val.unwrapErr(),\
            depth\
        };\
    }

#define PROPAGATE_FIND(val) \
    { auto val__ = val; PROPAGATE_FIND_ERR(val__); return val__.unwrap(); }

#define PROPAGATE_FINDERROR(val) \
    { auto val__ = val; if (!val__) { return val__.unwrapErr().error; } return val__.unwrap(); }

static auto convertToNS = +[](std::shared_ptr<Entity> const& ent) {
    return std::static_pointer_cast<const Namespace>(ent);
};

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
    auto name = getName();
    if (m_namespace) {
        return m_namespace->getFullName() + "::" + name;
    }
    return name;
}

std::string Entity::getName() const {
    return m_name.size() ? m_name : "`anonymous`";
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

void TypeEntity::dump(size_t indent) {
    m_instance.getShared().logDebug(
        std::string(indent, ' ') + 
        "TypeEntity \"" + getFullName() + "\""
        ", extern: " + std::to_string(m_isExtern) +
        ", type: `" + type->toString() + "`"
    );
}


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

void Variable::dump(size_t indent) {
    m_instance.getShared().logDebug(
        std::string(indent, ' ') + 
        "Variable \"" + getFullName() + "\", type: `" + type.toString() + 
        "`, extern: " + std::to_string(m_isExtern) + ", value: " +
        std::to_string(reinterpret_cast<uintptr_t>(value.get())) + ", decl: " +
        std::to_string(reinterpret_cast<uintptr_t>(declaration))
    );
}


FunctionEntity::FunctionEntity(
    Instance& instance,
    std::shared_ptr<Namespace> container,
    std::string const& name,
    QualifiedFunType const& type,
    ast::AFunctionDeclStmt* decl,
    bool isExtern
) : ValueEntity(instance, container, name, EntityType::Function, isExtern),
    type(type), declaration(decl) {}

void FunctionEntity::dump(size_t indent) {
    m_instance.getShared().logDebug(
        std::string(indent, ' ') + 
        "FunctionEntity \"" + getFullName() + "\", type: `" + type.toString() + 
        "`, extern: " + std::to_string(m_isExtern) + ", decl: " +
        std::to_string(reinterpret_cast<uintptr_t>(declaration))
    );
}


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
    bool isExtern
) : Entity(instance, container, name, EntityType::Namespace, isExtern),
    m_isGlobal(false) {}

void Namespace::dump(size_t indent) {
    m_instance.getShared().logDebug(
        std::string(indent, ' ') + 
        "Namespace \"" + getFullName() + "\", global: " + std::to_string(m_isGlobal) +
        ", extern: " + std::to_string(m_isExtern) +
        ", entities: "
    );
    for (auto& [_, entities] : m_entities) {
        for (auto& entity : entities) {
            entity->dump(indent + 4);
        }
    }
}

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
            return ns.unwrap()->pushEntity(descopedName, entity);
        }
        // make it then
        return makeEntity<Namespace>(
            nsName, m_isExtern
        )->pushEntity(descopedName, entity);
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
    Option<std::vector<Parameter>> const& parameters,
    bool expandExtern
) const {
    return getEntity(
        name, currentNamespace, testNamespaces, type, parameters, expandExtern
    ).isOk();
}

FindResult<Namespace> Namespace::getNamespaceMut(std::string const& name) {
    // does provided namespace exist?
    if (!m_entities.count(name)) {
        return "No namespace found"s;
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
    return "No namespace found"s;
}

FindResult<const Namespace> Namespace::getNamespace(std::string const& name) const {
    // does provided namespace exist?
    if (!m_entities.count(name)) {
        return std::const_pointer_cast<Namespace>(
            shared_from_this()
        )->makeExtern(
            name, EntityType::Namespace, None, true
        ).map(convertToNS);
    }
    // find entity that is a namespace
    for (auto& ent : m_entities.at(name)) {
        if (
            ent->getType() == EntityType::Namespace ||
            ent->getType() == EntityType::Class
        ) {
            return std::static_pointer_cast<const Namespace>(ent);
        }
    }
    return std::const_pointer_cast<Namespace>(
        shared_from_this()
    )->makeExtern(name, EntityType::Namespace, None, true).map(convertToNS);
}

FindResult<const Namespace> Namespace::getNamespace(NamespaceParts const& name) const {
    auto descoped = shared_from_this();
    for (auto& ns : name) {
        auto d = descoped->getNamespace(ns);
        PROPAGATE_ERROR(d);
        descoped = d.unwrap();
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
            auto e = space.unwrap()->getEntities(descopedName, type);
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

ScopeFindResult<Entity> Namespace::getEntity(
    std::string const& rawName,
    Option<EntityType> const& type,
    Option<std::vector<Parameter>> const& parameters,
    size_t depth,
    bool expandExtern
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
            auto e = space.unwrap()->getEntity(
                descopedName, type, parameters, depth + 1, expandExtern
            );
            if (!e) {
                return FindError {
                    "Namespace or class \"" + ns + "\" does not contain "
                    "\"" + name + "\"",
                    depth
                };
            }
            return e;
        }
        return FindError {
            "Namespace or class \"" + ns + "\" not found",
            depth
        };
    }

    // are there any entities with this name?
    if (!m_entities.count(name)) {
        PROPAGATE_FIND(
            std::const_pointer_cast<Namespace>(
                shared_from_this()
            )->makeExtern(name, type, parameters, expandExtern)
        );
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
            PROPAGATE_FIND(
                std::const_pointer_cast<Namespace>(
                    shared_from_this()
                )->makeExtern(name, type, parameters, expandExtern)
            );
        }
        return m_entities.at(name).front();
    }

    bool overloadError = false;
    
    // match overload
    for (auto& ent : m_entities.at(name)) {
        switch (ent->getType()) {
            case EntityType::Function: {
                auto fun = std::static_pointer_cast<FunctionEntity>(ent);
                if (fun->type.type->matchParameters(parameters.value())) {
                    return ent;
                } else {
                    overloadError = true;
                }
            } break;

            default: break;
        }
    }

    // none found then
    auto ext = std::const_pointer_cast<Namespace>(
        shared_from_this()
    )->makeExtern(name, type, parameters, expandExtern);

    if (ext || !overloadError) {
        PROPAGATE_FIND(ext);
    }

    return FindError { 
        "No matching overload found",
        depth
    };
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
        while (descoped = descoped->getNamespace(ns).unwrapOr(
            std::shared_ptr<Namespace>(nullptr)
        )) {
            // look for entity in descoped namespace
            auto e = descoped->getEntities(name, type);
            entities.insert(entities.end(), e.begin(), e.end());
        }
    }
    
    // what about extra using namespaces?
    for (auto& test : testNamespaces) {
        if (auto space = getNamespace(test)) {
            auto e = space.unwrap()->getEntities(name, type);
            entities.insert(entities.end(), e.begin(), e.end());
        }
    }

    // none found
    return entities;
}

ScopeFindResult<Entity> Namespace::getEntity(
    std::string const& name,
    NamespaceParts const& currentNamespace,
    std::vector<NamespaceParts> const& testNamespaces,
    Option<EntityType> const& type,
    Option<std::vector<Parameter>> const& parameters,
    bool expandExtern
) const {
    // is this in current namespace?
    auto entInCurrent = getEntity(name, type, parameters, 0, expandExtern);
    if (entInCurrent) {
        return entInCurrent.unwrap();
    }
    // if the name is a full path, then do no other 
    // namespace resolution
    if (name.starts_with("::")) {
        return entInCurrent;
    }

    // check current namespaces
    auto descoped = shared_from_this();
    for (auto& ns : currentNamespace) {
        size_t depth = 1;
        // check if namespace exists
        while (descoped = descoped->getNamespace(ns).unwrapOr(
            std::shared_ptr<Namespace>(nullptr)
        )) {
            // look for entity in descoped namespace
            if (auto e = descoped->getEntity(name, type, parameters, depth, expandExtern)) {
                return e;
            }
            depth++;
        }
    }
    
    // what about extra using namespaces?
    for (auto& test : testNamespaces) {
        if (auto space = getNamespace(test)) {
            if (auto e = space.unwrap()->getEntity(
                name, type, parameters, test.size(), expandExtern
            )) {
                return e;
            }
        }
    }

    // none found
    return FindError {
        "Unknown identifier \"" + name + "\"",
        0
    };
}

FindResult<Entity> Namespace::makeExtern(
    std::string const& name,
    Option<EntityType> const& type,
    Option<std::vector<Parameter>> const& parameters,
    bool expandExtern
) {
    if (!m_isExtern || !expandExtern) {
        return ""s;
    }
    if (parameters || (type && type.value() == EntityType::Function)) {
        return std::static_pointer_cast<Entity>(
            makeEntity<FunctionEntity>(
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
            )
        );
    }
    if (!type) {
        return "No type provided to infer extern entity from"s;
    }

    switch (type.value()) {
        case EntityType::Class: return std::static_pointer_cast<Entity>(
            makeEntity<Class>(
                name,
                m_instance.getCompiler().makeType<ClassType>(name),
                true
            )
        );

        case EntityType::Variable: return std::static_pointer_cast<Entity>(
            makeEntity<Variable>(
                name, 
                QualifiedType(
                    m_instance.getCompiler().makeType<ExternType>()
                ),
                nullptr, nullptr, true
            )
        );

        case EntityType::Type: return std::static_pointer_cast<Entity>(
            makeEntity<TypeEntity>(
                name, m_instance.getCompiler().makeType<BuiltInType>(
                    types::DataType::Void
                ), true
            )
        );

        case EntityType::Namespace: return std::static_pointer_cast<Entity>(
            makeEntity<Namespace>(
                name, true
            )
        );

        default: break;
    }
    return "Unable to infer extern entity"s;
}

std::string Namespace::getName() const {
    return m_isGlobal ? "" : Entity::getName();
}


Class::Class(
    Instance& instance,
    std::shared_ptr<Namespace> container,
    std::string const& name,
    std::shared_ptr<ClassType> classType,
    bool isExtern
) : Namespace(instance, container, name, isExtern),
    m_classType(classType), m_complete(false)
{
    m_type = EntityType::Class;
}

bool Class::isComplete() const {
    return m_complete;
}

void Class::markComplete() {
    m_complete = true;
}

void Class::dump(size_t indent) {
    m_instance.getShared().logDebug(
        std::string(indent, ' ') + 
        "Class \"" + getFullName() + "\", extern: " + std::to_string(m_isExtern) +
        ", entities: "
    );
    for (auto& [_, entities] : m_entities) {
        for (auto& entity : entities) {
            entity->dump(indent + 4);
        }
    }
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

bool Class::hasMember(
    std::string const& name,
    bool expandExtern
) const {
    return hasEntity(
        name, {}, {}, EntityType::Variable, None, expandExtern
    );
}

bool Class::hasMemberFunction(
    std::string const& name,
    Option<std::vector<Parameter>> const& parameters,
    bool expandExtern
) const {
    return hasEntity(
        name, {}, {}, EntityType::Function, parameters, expandExtern
    );
}

std::shared_ptr<Variable> Class::getMember(
    std::string const& name,
    bool expandExtern
) const {
    return std::static_pointer_cast<Variable>(
        getEntity(name, {}, {}, EntityType::Variable, None, expandExtern).unwrapOr(
            std::shared_ptr<Entity>(nullptr)
        )
    );
}

std::shared_ptr<FunctionEntity> Class::getMemberFunction(
    std::string const& name,
    Option<std::vector<Parameter>> const& parameters,
    bool expandExtern
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
            getEntity(name, {}, {}, EntityType::Function, paramsWithThis, expandExtern).unwrapOr(
                std::shared_ptr<Entity>(nullptr)
            )
        )) {
            return memberFun;
        }
    }
    return std::static_pointer_cast<FunctionEntity>(
        getEntity(name, {}, {}, EntityType::Function, parameters, expandExtern).unwrapOr(
            std::shared_ptr<Entity>(nullptr)
        )
    );
}

