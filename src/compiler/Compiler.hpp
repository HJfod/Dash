#pragma once

#include <utils/Types.hpp>
#include "Type.hpp"
#include <unordered_set>

namespace gdml {
    enum class ConstValue {
        True,
        False,
        Zero,
        EmptyString,
        Null,
    };
    constexpr size_t CONST_VALUE_COUNT = 5;

    struct Scope {
    public:
        enum class Search {
            Found,
            NoMatchingOverload,
            NotFound,
        };

    protected:
        Compiler& m_compiler;
        std::vector<std::string> m_namespace;
        std::unordered_map<std::string, std::shared_ptr<Type>> m_types;
        std::unordered_map<std::string, std::shared_ptr<Variable>> m_variables;
        std::unordered_map<std::string, std::vector<std::shared_ptr<FunctionEntity>>> m_functions;

        Scope(Compiler& compiler);

        friend class Compiler;

    public:
        Compiler& getCompiler();

        // types

        void pushType(std::string const& name, std::shared_ptr<Type> type);
        bool hasType(std::string const& name) const;
        std::shared_ptr<Type> getType(std::string const& name) const;

        // entities

        bool hasEntity(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        );
        std::shared_ptr<Entity> getEntity(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        );

        // variables

        std::shared_ptr<Variable> pushVariable(
            std::string const& name,
            std::shared_ptr<Variable> var
        );
        bool hasVariable(std::string const& name) const;
        std::shared_ptr<Variable> getVariable(std::string const& name);

        // functions

        std::shared_ptr<FunctionEntity> pushFunction(
            std::string const& name,
            std::shared_ptr<FunctionEntity> fun
        );
        Search hasFunction(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        std::shared_ptr<FunctionEntity> getFunction(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        );

        // namespaces

        void pushNameSpace(std::string const& name);
        void popNameSpace();
        std::string getNameSpace() const;
    };

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

    class Compiler {
    protected:
        Instance& m_instance;
        ast::AST* m_ast;
        std::unordered_set<Value*> m_values;
        std::vector<Scope> m_scope;
        std::unordered_map<ConstValue, Value*> m_constValues;
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

        bool hasType(std::string const& name) const;
        std::shared_ptr<Type> getType(std::string const& name) const;

        bool hasEntity(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        );
        std::shared_ptr<Entity> getEntity(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        );

        bool hasVariable(std::string const& name) const;
        std::shared_ptr<Variable> getVariable(std::string const& name);

        Scope::Search hasFunction(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        std::shared_ptr<FunctionEntity> getFunction(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        );

        template<class T = Type, class... Args>
        std::shared_ptr<T> makeType(Args... args) {
            return std::make_shared<T>(*this, std::forward<Args>(args)...);
        }

        template<class T>
        std::shared_ptr<Type> getBuiltInType() const {
            return m_scope.back().getType(types::dataTypeToString(types::getDataType<T>()));
        }
        std::shared_ptr<Type> getBuiltInType(types::DataType type) const;

        template<class T = Value, class... Args>
        T* makeValue(
            Args... args
        ) {
            auto value = new T(*this, std::forward<Args>(args)...);
            m_values.insert(value);
            return value;
        }

        Value* getConstValue(ConstValue value) const;

        void codegen(std::ostream& stream) const noexcept;
    };
}
