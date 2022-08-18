#pragma once

#include <utils/Types.hpp>
#include "Type.hpp"

namespace gdml {
    class Value {
    protected:
        Compiler& m_compiler;

    public:
        Value(Compiler& compiler);

        virtual Value* copy() = 0;
        virtual ~Value() = default;
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

    struct Entity {
        std::string fullName;

        virtual QualifiedType getType() const = 0;
        virtual Value* eval(Instance& instance) = 0;
        virtual ~Entity() = default;
    };

    struct Variable : public Entity {
        QualifiedType type;
        Value* value = nullptr;
        ast::VariableDeclExpr* declaration = nullptr;

        QualifiedType getType() const override {
            return type;
        }
        Value* eval(Instance& instance) override {
            return value;
        }

        Variable(
            QualifiedType const& type,
            Value* value,
            ast::VariableDeclExpr* decl
        );
    };

    struct FunctionEntity : public Entity {
        QualifiedFunType type;
        ast::FunctionDeclStmt* declaration = nullptr;

        QualifiedType getType() const override {
            return type.into<Type>();
        }
        Value* eval(Instance& instance) override;

        FunctionEntity(
            QualifiedFunType const& type,
            ast::FunctionDeclStmt* decl
        );
    };
}
