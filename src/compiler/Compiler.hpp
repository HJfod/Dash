#pragma once

#include <utils/Types.hpp>

namespace gdml {
    class GDML;
    class Compiler;
    
    namespace ast {
        class AST;
    }

    class Value;
    
    class Type {
    protected:
        Compiler& m_compiler;
        const types::DataType m_type;
    
        Type(Compiler& compiler, const types::DataType type);

        friend class Compiler;

    public:
        const types::DataType getType() const;

        virtual Value* instantiate(TypeQualifiers const& qualifiers);
        virtual std::string codegen() const;

        virtual ~Type() = default;
    };

    class ArrayType : public Type {
    protected:
        Type* m_inner;
        size_t m_size; // 0 for vector

        ArrayType(Type* inner, size_t size);

        friend class Compiler;

    public:
        Type* getInnerType();

        std::string codegen() const override;
    };

    class ClassType : public Type {
    protected:
        std::string m_name;
        std::unordered_map<std::string, Type*> m_members;

        ClassType();

        friend class Compiler;

    public:
        inline void addMember(std::string const& name, Type* type) {
            m_members.insert({ name, type });
        }

        std::string const& getName() const;
        std::unordered_map<std::string, Type*> const& getMembers() const;

        std::string codegen() const override;
    };

    struct QualifiedType {
        Type* type;
        TypeQualifiers qualifiers;
    };

    class Value {
    protected:
        QualifiedType m_type;

    public:
        Value(QualifiedType const& type);

        virtual Value* copy() = 0;
        virtual ~Value() = default;
    };

    template<class T>
    class BuiltInValue : public Value {
    protected:
        T m_value;
    
    public:
        BuiltInValue(
            T const& value,
            TypeQualifiers const& type
        ) : Value({ type }),
            m_value(value) {}

        Value* copy() override {
            
        }
        T getValue() const {
            return m_value;
        }
        void setValue(T const& value) {
            m_value = value;
        }
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
        std::unordered_map<std::string, Type*> m_types;
        std::unordered_map<size_t, Value*> m_values;
        std::vector<std::string> m_scope;
        Formatter m_formatter;
    
        void loadBuiltinTypes();

        friend class GDML;

    public:
        Compiler(Instance& instance, ast::AST* ast);
        Error compile();

        Instance& getInstance() const;
        Formatter& getFormatter();

        void pushScope(std::string const& name);
        void popScope(std::string const& name);
        std::vector<std::string> const& getScope() const;

        template<class T = Type, class... Args>
        T* makeType(
            std::string const& name,
            Args... args
        ) {
            auto type = new T(*this, name, std::forward<Args>(args)...);
            m_types.insert({ name, type });
            return type;
        }

        template<class T = Value, class... Args>
        T* makeValue(
            Args... args
        ) {

        }

        bool typeExists(std::string const& name) const;
        Type* getType(std::string const& name) const;

        void codegen(std::ostream& stream) const noexcept;
    };
}
