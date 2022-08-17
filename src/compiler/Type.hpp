#pragma once

#include <utils/Types.hpp>

namespace gdml {
    class Compiler;
    class Value;
    
    namespace ast {
        class AST;
    }

    class Type {
    protected:
        Compiler& m_compiler;
        const types::DataType m_type;
        std::unordered_map<std::string, std::string> m_casts;

        friend class Compiler;
    
    public:
        Type(Compiler& compiler, const types::DataType type);

        const types::DataType getType() const;
    
        virtual bool convertibleTo(std::shared_ptr<Type> other) const;
        bool castableTo(std::shared_ptr<Type> other) const;
        std::string getCastOperatorFor(std::shared_ptr<Type> type) const;
        void addCastOperatorFor(std::shared_ptr<Type> type, std::string const& op);

        virtual Value* instantiate();
        virtual std::string codegenName() const;
        virtual std::string toString() const;

        virtual ~Type() = default;
    };

    template<class T>
    struct TQualifiedType {
        std::shared_ptr<T> type = nullptr;
        types::TypeQualifiers qualifiers = types::NON_CONST_QUALIFIED;

        TQualifiedType() = default;
        TQualifiedType(
            std::shared_ptr<T> type,
            types::TypeQualifiers qualifiers = types::NON_CONST_QUALIFIED
        ) : type(type), qualifiers(qualifiers) {}

        template<class T2>
        TQualifiedType<T2> into() const {
            return TQualifiedType<T2>(
                std::static_pointer_cast<T2>(type),
                qualifiers
            );
        }

        bool convertibleTo(TQualifiedType<T> const& other) const {
            if (!type) return false;
            return type->convertibleTo(other.type);
        }

        bool castableTo(TQualifiedType<T> const& other) const {
            if (qualifiers.isConst && !other.qualifiers.isConst) {
                return false;
            }
            return type->castableTo(other.type);
        }

        std::string codegenName() const {
            std::string res {};
            res += type ? type->codegenName() : "auto";
            if (qualifiers.isConst) {
                res += " const";
            }
            return res;
        }

        std::string toString() const {
            std::string res {};
            res += type ? type->toString() : "auto";
            if (qualifiers.isConst) {
                res += " const";
            }
            return res;
        }
    };
    using QualifiedType = TQualifiedType<Type>;

    class FunctionType : public Type {
    protected:
        QualifiedType m_returnType;
        std::vector<QualifiedType> m_parameters;

    public:
        FunctionType(
            Compiler& compiler,
            QualifiedType const& returnType,
            std::vector<QualifiedType> const& parameters
        );

        QualifiedType const& getReturnType();
        void setReturnType(QualifiedType const& type);
        
        std::vector<QualifiedType> const& getParameters();

        bool matchParameters(std::vector<QualifiedType> const& parameters) const;

        std::string codegenName() const override;
        std::string toString() const override;
    };

    class ArrayType : public Type {
    protected:
        QualifiedType m_inner;
        size_t m_size; // 0 for vector

        friend class Compiler;

    public:
        ArrayType(Compiler& compiler, QualifiedType const& inner, size_t size);

        QualifiedType const& getInnerType();

        std::string codegenName() const override;
        std::string toString() const override;
    };

    class ClassType : public Type {
    protected:
        std::string m_name;
        std::unordered_map<std::string, std::shared_ptr<Type>> m_members;

        ClassType(Compiler& compiler, std::string const& name);

        friend class Compiler;

    public:
        inline void addMember(std::string const& name, std::shared_ptr<Type> type) {
            m_members.insert({ name, type });
        }

        std::string const& getName() const;
        std::unordered_map<std::string, std::shared_ptr<Type>> const& getMembers() const;

        std::string codegenName() const override;
        std::string toString() const override;
    };

    class PointerType : public Type {
    protected:
        QualifiedType m_inner;
        types::PointerType m_pointerType;
    
        friend class Compiler;

    public:
        PointerType(Compiler& compiler, QualifiedType const& inner, types::PointerType pointerType);

        QualifiedType const& getInnerType();

        std::string codegenName() const override;
        std::string toString() const override;
    };

    using QualifiedFunType = TQualifiedType<FunctionType>;
}
