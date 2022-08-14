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
        const types::DataType m_type;
    
        Type(const types::DataType type);

        friend class Compiler;

    public:
        types::DataType getType() const;

        virtual Value* instantiate(TypeQualifiers const& qualifiers);

        virtual ~Type() = default;
    };

    class ArrayType : public Type {
    protected:
        Type* m_inner;

        ArrayType(Type* inner);

        friend class Compiler;

    public:
        Type* getInnerType();
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
    };

    struct QualifiedType {
        Type* type;
        TypeQualifiers qualifiers;
        bool isModifiable;
    };

    class Value {
    protected:
        QualifiedType m_type;

    public:
        Value(QualifiedType const& type);

        virtual ~Value() = default;
    };

    template<class T>
    class BuiltInValue : public Value {
    protected:
        T m_value;
    
    public:
        BuiltInValue(
            T const& value,
            TypeQualifiers const& type,
            bool isModifiable
        ) : Value({ type, isModifiable }),
            m_value(value) {}

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
            auto type = new T(name, std::forward<Args>(args)...);
            m_types.insert({ name, type });
            return type;
        }

        bool typeExists(std::string const& name) const;
        Type* getType(std::string const& name) const;

        void codegen(std::ostream& stream) const noexcept;
    };
}
