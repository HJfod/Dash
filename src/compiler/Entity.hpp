#pragma once

#include <utils/Types.hpp>
#include "Type.hpp"

namespace gdml {
    struct Entity {
    protected:
        std::shared_ptr<Namespace> m_namespace;
        std::string m_name;
        EntityType m_type;

    public:
        Entity(
            std::shared_ptr<Namespace> container,
            std::string const& name,
            EntityType type
        );

        EntityType getType() const;
        std::string getFullName() const;
        bool hasParentNamespace() const;

        virtual bool isValue() const {
            return false;
        }
        virtual ~Entity() = default;
    };

    struct TypeEntity : public Entity {
        std::shared_ptr<Type> type;

        TypeEntity(
            std::shared_ptr<Namespace> container,
            std::string const& name,
            std::shared_ptr<Type> type
        );
    };

    struct ValueEntity : public Entity {
        virtual QualifiedType getValueType() const = 0;
        virtual std::shared_ptr<Value> eval(Instance& instance) = 0;

        bool isValue() const override {
            return true;
        }
        ValueEntity(
            std::shared_ptr<Namespace> container, 
            std::string const& name,
            EntityType type
        );
    };

    struct Variable : public ValueEntity {
        QualifiedType type;
        std::shared_ptr<Value> value = nullptr;
        ast::VariableDeclExpr* declaration = nullptr;

        QualifiedType getValueType() const override {
            return type;
        }
        std::shared_ptr<Value> eval(Instance&) override {
            return value;
        }

        Variable(
            std::shared_ptr<Namespace> container,
            std::string const& name,
            QualifiedType const& type,
            std::shared_ptr<Value> value,
            ast::VariableDeclExpr* decl
        );
    };

    struct FunctionEntity : public ValueEntity {
        QualifiedFunType type;
        ast::FunctionDeclStmt* declaration = nullptr;

        QualifiedType getValueType() const override {
            return type.into<Type>();
        }
        std::shared_ptr<Value> eval(Instance& instance) override;

        FunctionEntity(
            std::shared_ptr<Namespace> container,
            std::string const& name,
            QualifiedFunType const& type,
            ast::FunctionDeclStmt* decl
        );
    };

    struct Namespace :
        public Entity,
        public std::enable_shared_from_this<Namespace>
    {
    protected:
        bool m_isGlobal;
        std::unordered_map<std::string, std::vector<std::shared_ptr<Entity>>> m_entities;

        void pushEntity(std::string const& name, std::shared_ptr<Entity> entity);
        
        std::shared_ptr<Entity> getEntity(
            std::string const& name,
            Option<EntityType> const& type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        std::shared_ptr<const Namespace> getNamespace(std::string const& name) const;
        std::shared_ptr<const Namespace> getNamespace(NamespaceParts const& name) const;

    public:
        Namespace(
            std::shared_ptr<Namespace> container,
            std::string const& name,
            bool isGlobal = false
        );

        bool isGlobal() const;

        bool hasEntity(
            std::string const& name,
            NamespaceParts const& currentNamespace,
            std::vector<NamespaceParts> const& testNamespaces,
            Option<EntityType> const& type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        std::shared_ptr<Entity> getEntity(
            std::string const& name,
            NamespaceParts const& currentNamespace,
            std::vector<NamespaceParts> const& testNamespaces,
            Option<EntityType> const& type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        template<class T, class... Args>
        std::shared_ptr<T> makeEntity(
            std::string const& name, Args&&... args
        ) {
            auto entity = std::make_shared<T>(
                shared_from_this(),
                name, std::forward<Args>(args)...
            );
            pushEntity(name, entity);
            return entity;
        }
    };
}
