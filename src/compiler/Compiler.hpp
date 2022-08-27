#pragma once

#include <utils/Types.hpp>
#include "Type.hpp"
#include <unordered_set>
#include "Entity.hpp"
#include <utils/Macros.hpp>

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

    class ASTParser {
    protected:
        Compiler& m_compiler;
        size_t m_inExtern;
    
    public:
        ASTParser(Compiler& compiler);

        bool isExtern() const;
        void pushExtern();
        void popExtern();
    };

    struct Scope {
    protected:
        Compiler& m_compiler;
        std::shared_ptr<Namespace> m_global;
        NamespaceParts m_currentNamespace;
        std::vector<NamespaceParts> m_namespaces;

        friend class Compiler;

    public:
        Scope(Compiler& compiler);

        void useNamespace(NamespaceParts const& space);

        void pushNamespace(std::string const& name);
        void popNamespace();
        std::string currentNamespace() const;

        bool hasEntity(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<Parameter>> const& parameters,
            bool expandExtern
        ) const;
        ScopeFindResult<Entity> getEntity(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<Parameter>> const& parameters,
            bool expandExtern
        ) const;
        template<class T, class... Args>
        std::shared_ptr<T> makeEntity(
            std::string const& name, Args&&... args
        ) {
            return m_global->makeEntity<T, Args...>(
                currentNamespace() + name, std::forward<Args>(args)...
            );
        }

        std::vector<std::shared_ptr<Entity>> getEntities(
            std::string const& name,
            Option<EntityType> type
        ) const;
    };

    class Compiler {
    protected:
        Instance& m_instance;
        ast::AST* m_ast;
        std::unordered_map<ConstValue, std::shared_ptr<Value>> m_constValues;
        std::vector<Scope> m_scope;
        Formatter m_formatter;
        ASTParser m_astParser;
    
        void loadBuiltinTypes();
        void loadConstValues();

        friend class GDML;

    public:
        Compiler(Instance& instance, ast::AST* ast);
        Error compile();

        ~Compiler();

        Instance& getInstance() const;
        Formatter& getFormatter();
        ASTParser& getASTParser();

        void pushScope();
        void popScope();
        Scope& getScope(size_t offset = 0);
        void dumpScopes();

        bool hasEntity(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<Parameter>> const& parameters,
            bool checkAllScopes = true,
            bool expandExtern = true
        ) const;
        template<class T>
        bool hasEntity(
            std::string const& name,
            Option<std::vector<Parameter>> const& parameters = None,
            bool checkAllScopes = true,
            bool expandExtern = true
        ) {
            if constexpr (std::is_same_v<T, Variable>) {
                return hasEntity(name, EntityType::Variable, None, checkAllScopes, expandExtern);
            }
            else if constexpr (std::is_same_v<T, FunctionEntity>) {
                return hasEntity(name, EntityType::Function, parameters, checkAllScopes, expandExtern);
            }
            else if constexpr (std::is_same_v<T, TypeEntity>) {
                return hasEntity(name, EntityType::Type, None, checkAllScopes, expandExtern);
            }
            else if constexpr (std::is_same_v<T, Namespace>) {
                return hasEntity(name, EntityType::Namespace, None, checkAllScopes, expandExtern);
            }
            else if constexpr (std::is_same_v<T, Class>) {
                return hasEntity(name, EntityType::Class, None, checkAllScopes, expandExtern);
            }
            else if constexpr (std::is_same_v<T, ValueEntity>) {
                return hasEntity(name, None, None, checkAllScopes, expandExtern);
            }
            else {
                static_assert(!std::is_same_v<T, T>, "Invalid type to check entity as");
            }
        }

        FindResult<Entity> getEntity(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<Parameter>> const& parameters,
            bool expandExtern = true
        ) const;
        template<class T>
        FindResult<T> getEntity(
            std::string const& name,
            Option<std::vector<Parameter>> const& parameters = None,
            bool expandExtern = true
        ) {
            if constexpr (std::is_same_v<T, Variable>) {
                return getEntityAs<T>(name, EntityType::Variable, None, expandExtern);
            }
            else if constexpr (std::is_same_v<T, FunctionEntity>) {
                return getEntityAs<T>(name, EntityType::Function, parameters, expandExtern);
            }
            else if constexpr (std::is_same_v<T, TypeEntity>) {
                return getEntityAs<T>(name, EntityType::Type, None, expandExtern);
            }
            else if constexpr (std::is_same_v<T, Namespace>) {
                return getEntityAs<T>(name, EntityType::Namespace, None, expandExtern);
            }
            else if constexpr (std::is_same_v<T, Class>) {
                return getEntityAs<T>(name, EntityType::Class, None, expandExtern);
            }
            else if constexpr (std::is_same_v<T, ValueEntity>) {
                return getEntityAs<T>(name, None, None, expandExtern);
            }
            else {
                static_assert(!std::is_same_v<T, T>, "Invalid type to get entity as");
            }
        }
        template<class T>
        FindResult<T> getEntityAs(
            std::string const& name,
            Option<EntityType> type,
            Option<std::vector<Parameter>> const& parameters,
            bool expandExtern = true
        ) const {
            auto e = getEntity(name, type, parameters, expandExtern);
            PROPAGATE_ERROR(e);
            return std::static_pointer_cast<T>(e.unwrap());
        }

        std::vector<std::shared_ptr<Entity>> getEntities(
            std::string const& name,
            Option<EntityType> type
        ) const;

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
