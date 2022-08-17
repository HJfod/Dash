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
        bool m_blocking;
        std::unordered_map<std::string, std::shared_ptr<Type>> m_types;
        std::unordered_map<std::string, Variable> m_variables;
        std::unordered_map<std::string, std::vector<FunctionEntity>> m_functions;

        Scope(Compiler& compiler, bool blocking);

        friend class Compiler;

    public:
        Compiler& getCompiler();

        void pushType(std::string const& name, std::shared_ptr<Type> type);
        bool hasType(std::string const& name) const;

        Variable* pushVariable(std::string const& name, Variable const& var);
        bool hasVariable(std::string const& name) const;
        Variable* getVariable(std::string const& name);

        FunctionEntity* pushFunction(std::string const& name, FunctionEntity const& fun);
        Search hasFunction(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;
        FunctionEntity* getFunction(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        );
    };

    class Formatter {
    protected:
        Compiler& m_compiler;
        size_t m_indentation = 0;
    
    public:
        Formatter(Compiler& compiler);

        void pushIndent();
        void popIndent();
        void newline(std::ostream& stream) const;
    };

    class Compiler {
    protected:
        Instance& m_instance;
        ast::AST* m_ast;
        std::unordered_set<Value*> m_values;
        std::vector<Scope> m_scope;
        std::vector<std::string> m_namespace;
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

        void pushNameSpace(std::string const& name);
        void popNameSpace(std::string const& name);
        std::vector<std::string> const& getNameSpaceStack() const;
        std::string getNameSpace() const;

        void pushScope(bool blocking);
        void popScope();
        Scope& getScope(size_t offset = 0);

        // entities

        Entity* getEntity(std::string const& name);

        Variable* getVariable(std::string const& name);
        bool variableExists(std::string const& name) const;

        FunctionEntity* getFunction(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        );
        bool functionExists(
            std::string const& name,
            Option<std::vector<QualifiedType>> const& parameters
        ) const;

        // types

        template<class T = Type, class... Args>
        std::shared_ptr<T> makeType(Args... args) {
            auto type = std::make_shared<T>(*this, std::forward<Args>(args)...);
            return type;
        }

        bool typeExists(std::string const& name) const;
        std::shared_ptr<Type> getType(std::string const& name) const;
        template<class T>
        std::shared_ptr<Type> getBuiltInType() const {
            return getType(types::dataTypeToString(types::getDataType<T>()));
        }
        std::shared_ptr<Type> getBuiltInType(types::DataType type) const;

        // values

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
