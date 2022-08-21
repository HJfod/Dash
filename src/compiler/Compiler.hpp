#pragma once

#include <utils/Types.hpp>
#include "Type.hpp"
#include <unordered_set>
#include "Entity.hpp"

namespace gdml {
    enum class ConstValue {
        True,
        False,
        Zero,
        EmptyString,
        Null,
    };
    constexpr size_t CONST_VALUE_COUNT = 5;

    class Formatter {
    protected:
        Compiler& m_compiler;
        size_t m_indentation = 0;
        bool m_skipSemiColon = false;
    
    public:
        Formatter(Compiler& compiler);

        void pushIndent();
        void popIndent();
        void newLine(std::ostream& stream) const;
        void semiColon(std::ostream& stream);
        void skipSemiColon();
    };

    struct Scope {
    protected:
        std::shared_ptr<Namespace> m_global;
        NamespaceParts m_currentNamespace;
        std::vector<NamespaceParts> m_namespaces;

        friend class Compiler;

    public:
        Scope(bool isGlobal);

        void useNamespace(NamespaceParts const& space);

        void pushNamespace(std::string const& name);
        void popNamespace();
        std::string currentNamespace() const;

        bool hasEntity(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        std::shared_ptr<Entity> getEntity(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        template<class T, class... Args>
        std::shared_ptr<T> makeEntity(
            std::string const& name, Args&&... args
        ) {
            return m_global->makeEntity<T, Args...>(
                currentNamespace() + name, std::forward<Args>(args)...
            );
        }
    };

    class Compiler {
    protected:
        Instance& m_instance;
        ast::AST* m_ast;
        std::unordered_map<ConstValue, std::shared_ptr<Value>> m_constValues;
        std::vector<Scope> m_scope;
        Formatter m_formatter;
    
        void loadBuiltinTypes();
        void loadConstValues();

        friend class GDML;

    public:
        Compiler(Instance& instance, ast::AST* ast);
        Error compile();

        ~Compiler();

        Instance& getInstance() const;
        Formatter& getFormatter();

        void pushScope();
        void popScope();
        Scope& getScope(size_t offset = 0);

        bool hasEntity(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        template<class T>
        bool hasEntity(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters = None
        ) {
            if constexpr (std::is_same_v<T, Variable>) {
                return hasEntityAs<T>(name, EntityType::Variable, None);
            }
            else if constexpr (std::is_same_v<T, FunctionEntity>) {
                return hasEntityAs<T>(name, EntityType::Function, parameters);
            }
            else if constexpr (std::is_same_v<T, TypeEntity>) {
                return hasEntityAs<T>(name, EntityType::Type, None);
            }
            else if constexpr (std::is_same_v<T, Namespace>) {
                return hasEntityAs<T>(name, EntityType::Namespace, None);
            }
            else if constexpr (std::is_same_v<T, ValueEntity>) {
                return hasEntityAs<T>(name, None, None);
            }
            else {
                static_assert(!std::is_same_v<T, T>, "Invalid type to check entity as");
            }
        }
        template<class T>
        bool hasEntityAs(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const {
            return hasEntity(
                name, type, parameters
            );
        }

        std::shared_ptr<Entity> getEntity(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        template<class T>
        std::shared_ptr<T> getEntity(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters = None
        ) {
            if constexpr (std::is_same_v<T, Variable>) {
                return getEntityAs<T>(name, EntityType::Variable, None);
            }
            else if constexpr (std::is_same_v<T, FunctionEntity>) {
                return getEntityAs<T>(name, EntityType::Function, parameters);
            }
            else if constexpr (std::is_same_v<T, TypeEntity>) {
                return getEntityAs<T>(name, EntityType::Type, None);
            }
            else if constexpr (std::is_same_v<T, Namespace>) {
                return getEntityAs<T>(name, EntityType::Namespace, None);
            }
            else if constexpr (std::is_same_v<T, ValueEntity>) {
                return getEntityAs<T>(name, None, None);
            }
            else {
                static_assert(!std::is_same_v<T, T>, "Invalid type to get entity as");
            }
        }
        template<class T>
        std::shared_ptr<T> getEntityAs(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<QualifiedType>> const& parameters
        ) const {
            return std::static_pointer_cast<T>(getEntity(
                name, type, parameters
            ));
        }

        template<class T = Type, class... Args>
        std::shared_ptr<T> makeType(Args... args) {
            return std::make_shared<T>(*this, std::forward<Args>(args)...);
        }

        template<class T>
        std::shared_ptr<BuiltInType> getBuiltInType() const {
            return std::static_pointer_cast<BuiltInType>(
                m_scope.back().getType(types::dataTypeToString(types::getDataType<T>()))
            );
        }
        std::shared_ptr<BuiltInType> getBuiltInType(types::DataType type) const;

        template<class T = Value, class... Args>
        std::shared_ptr<T> makeValue(
            Args&&... args
        ) {
            return std::make_shared<T>(*this, std::forward<Args>(args)...);
        }

        std::shared_ptr<Value> getConstValue(ConstValue value) const;

        void codegen(std::ostream& stream) const noexcept;
    };
}
