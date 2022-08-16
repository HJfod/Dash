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

    class Value {
    protected:
        Compiler& m_compiler;

    public:
        Value(Compiler& compiler);

        virtual Value* copy() = 0;
        virtual ~Value() = default;
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

    struct NamedEntity {
        QualifiedType type;
        Value* value = nullptr;
        ast::Stmt* declaration = nullptr;

        NamedEntity(
            QualifiedType const& type,
            Value* value,
            ast::Stmt* decl
        ) : type(type), value(value), declaration(decl) {}

        ~NamedEntity() {}
    };

    struct Scope {
        Compiler& compiler;
        std::vector<std::shared_ptr<Type>> types;
        std::unordered_map<std::string, std::shared_ptr<Type>> namedTypes;
        std::unordered_map<std::string, NamedEntity> variables;

        Scope(Compiler& compiler);

        void pushType(std::shared_ptr<Type> type);
        void pushNamedType(std::string const& name, std::shared_ptr<Type> type);

        NamedEntity* pushVariable(std::string const& name, NamedEntity const& var);
        bool hasVariable(std::string const& name) const;
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

        void pushScope();
        void popScope();
        Scope& getScope();

        template<class T = Type, class... Args>
        std::shared_ptr<T> makeNamedType(
            std::string const& name,
            Args... args
        ) {
            auto type = std::make_shared<T>(*this, std::forward<Args>(args)...);
            m_scope.back().pushNamedType(name, type);
            return type;
        }

        template<class T = Type, class... Args>
        std::shared_ptr<T> makeType(Args... args) {
            auto type = std::make_shared<T>(*this, std::forward<Args>(args)...);
            m_scope.back().pushType(type);
            return type;
        }

        template<class T = Value, class... Args>
        T* makeValue(
            Args... args
        ) {
            auto value = new T(*this, std::forward<Args>(args)...);
            m_values.insert(value);
            return value;
        }

        NamedEntity const* getVariable(std::string const& name) const;
        bool variableExists(std::string const& name) const;

        bool typeExists(std::string const& name) const;
        std::shared_ptr<Type> getType(std::string const& name) const;
        template<class T>
        std::shared_ptr<Type> getBuiltInType() const {
            return getType(types::dataTypeToString(types::getDataType<T>()));
        }
        std::shared_ptr<Type> getBuiltInType(types::DataType type) const;

        Value* getConstValue(ConstValue value) const;

        void codegen(std::ostream& stream) const noexcept;
    };

    // this has to be defined after Compiler 
    // because it uses it
    template<class T>
    class BuiltInValue : public Value {
    protected:
        T m_value;
    
    public:
        BuiltInValue(
            Compiler& compiler,
            T const& value
        ) : Value(compiler),
            m_value(value) {}

        Value* copy() override {
            return m_compiler.makeValue<BuiltInValue<T>>(m_value);
        }
        T getValue() const {
            return m_value;
        }
        void setValue(T const& value) {
            m_value = value;
        }
    };

    class PointerValue : public Value {
    protected:
        Value* m_value;
    
    public:
        PointerValue(Compiler& compiler, Value* value);

        Value* copy() override;

        Value* getValue() const;
        void setValue(Value* value);
    };
}
