#pragma once

#include <utils/Types.hpp>

namespace gdml {
    using FunctionTypeOverloads = std::vector<std::shared_ptr<FunctionType>>;
    
    enum class ImplStatus {
        None,
        Exists,
        Constexpr,
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

        bool convertibleTo(TQualifiedType<T> const& other, bool strict = false) const {
            if (!type) return false;
            return type->convertibleTo(other.type, strict);
        }

        bool codegenConvert(
            TQualifiedType<T> const& other,
            ast::ValueExpr* target,
            std::ostream& stream
        ) const {
            return type->codegenConvert(other.type, target, stream);
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
            if (qualifiers.refType == types::ReferenceType::Reference) {
                res += "&";
            }
            if (qualifiers.refType == types::ReferenceType::Move) {
                res += "&&";
            }
            return res;
        }

        std::string toString() const {
            std::string res {};
            res += type ? type->toString() : "auto";
            if (qualifiers.isConst) {
                res += " const";
            }
            if (qualifiers.refType == types::ReferenceType::Reference) {
                res += "&";
            }
            if (qualifiers.refType == types::ReferenceType::Move) {
                res += "&&";
            }
            return res;
        }
    
        static TQualifiedType<T> NO_TYPE;
    };

    class Type {
    protected:
        Compiler& m_compiler;
        const types::TypeClass m_class;

        friend class Compiler;
    
    public:
        Type(Compiler& compiler, const types::TypeClass type);

        const types::TypeClass getTypeClass() const;

        // typecasts

        virtual bool convertibleTo(std::shared_ptr<Type> other, bool strict) const = 0;
        virtual bool codegenConvert(
            std::shared_ptr<Type> other,
            ast::ValueExpr* target,
            std::ostream& stream
        ) const;
        virtual bool castableTo(std::shared_ptr<Type> other) const;
        virtual bool codegenCast(
            std::shared_ptr<Type> other,
            ast::ValueExpr* target,
            std::ostream& stream
        );

        // unary

        virtual ImplStatus implementsUnaryOperator(
            TokenType op,
            bool prefix
        ) const;
        virtual std::shared_ptr<Value> evalUnary(
            TokenType op,
            std::shared_ptr<Value> target,
            bool prefix
        );
        virtual bool codegenUnary(
            ast::UnaryExpr const* expr,
            std::ostream& stream
        ) const;

        // binary

        virtual ImplStatus implementsBinaryOperator(
            TokenType op,
            std::shared_ptr<Type> other
        ) const;
        virtual std::shared_ptr<Value> evalBinary(
            TokenType op,
            std::shared_ptr<Value> first,
            std::shared_ptr<Value> second
        );
        virtual bool codegenBinary(
            ast::BinaryExpr const* expr,
            std::ostream& stream
        ) const;

        virtual bool implementsMemberOperator() const;
        virtual TQualifiedType<Type> typeOfMember(
            std::string const& name,
            Option<std::vector<Parameter>> const& args
        ) const;
        
        virtual bool hasDefinition() const;

        virtual std::string codegenName() const = 0;
        virtual std::string toString() const = 0;

        virtual ~Type() = default;
    };
    using QualifiedType = TQualifiedType<Type>;

    struct Parameter {
        Option<std::string> name = None;
        QualifiedType type;

        Parameter() = default;
        Parameter(QualifiedType const& type);
        Parameter(std::string const& name, QualifiedType const& type);
        Parameter(ast::VariableDeclExpr* decl);

        std::string toString() const;
    };

    class BuiltInType : public Type {
    protected:
        const types::DataType m_type;
    
    public:
        BuiltInType(Compiler& compiler, const types::DataType type);

        const types::DataType getType() const;

        bool convertibleTo(std::shared_ptr<Type> other, bool strict) const override;
        bool codegenConvert(
            std::shared_ptr<Type> other,
            ast::ValueExpr* target,
            std::ostream& stream
        ) const override;
        bool castableTo(std::shared_ptr<Type> other) const override;
        bool codegenCast(
            std::shared_ptr<Type> other,
            ast::ValueExpr* target,
            std::ostream& stream
        ) override;

        ImplStatus implementsUnaryOperator(
            TokenType op,
            bool prefix
        ) const override;
        std::shared_ptr<Value> evalUnary(
            TokenType op,
            std::shared_ptr<Value> target,
            bool prefix
        ) override;
        bool codegenUnary(
            ast::UnaryExpr const* expr,
            std::ostream& stream
        ) const override;

        ImplStatus implementsBinaryOperator(
            TokenType op,
            std::shared_ptr<Type> other
        ) const override;
        std::shared_ptr<Value> evalBinary(
            TokenType op,
            std::shared_ptr<Value> first,
            std::shared_ptr<Value> second
        ) override;
        bool codegenBinary(
            ast::BinaryExpr const* expr,
            std::ostream& stream
        ) const override;

        bool implementsMemberOperator() const override;

        std::string codegenName() const override;
        std::string toString() const override;
    };

    class FunctionType : public Type {
    public:
        enum FunType {
            Normal,
            Member,
        };

    protected:
        QualifiedType m_returnType;
        std::vector<Parameter> m_parameters;
        FunType m_funType;

    public:
        FunctionType(
            Compiler& compiler,
            QualifiedType const& returnType,
            std::vector<Parameter> const& parameters,
            FunType funType = FunType::Normal
        );

        FunType getFunType() const;

        QualifiedType const& getReturnType();
        void setReturnType(QualifiedType const& type);

        std::vector<Parameter>& getParameters();
        void insertParameter(size_t index, Parameter const& parameter);
        bool matchParameters(std::vector<Parameter> const& parameters) const;

        bool convertibleTo(std::shared_ptr<Type> other, bool strict) const override;

        bool implementsMemberOperator() const override;

        std::string codegenName() const override;
        std::string toString() const override;
    };
    using QualifiedFunType = TQualifiedType<FunctionType>;

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
        std::shared_ptr<Class> m_entity = nullptr;

        friend class Compiler;
        friend struct Class;

    public:
        ClassType(Compiler& compiler, std::string const& name);

        std::string const& getName() const;

        std::shared_ptr<Class> getEntity();

        bool convertibleTo(std::shared_ptr<Type> other, bool strict) const override;

        QualifiedType typeOfMember(
            std::string const& name,
            Option<std::vector<Parameter>> const& args
        ) const override;
        bool hasDefinition() const override;

        std::string codegenName() const override;
        std::string toString() const override;
    };
    using QualifiedClassType = TQualifiedType<ClassType>;

    class PointerType : public Type {
    protected:
        QualifiedType m_inner;
    
        friend class Compiler;

    public:
        PointerType(Compiler& compiler, QualifiedType const& inner);

        QualifiedType const& getInnerType();

        bool convertibleTo(std::shared_ptr<Type> other, bool strict) const override;

        bool implementsMemberOperator() const override;

        std::string codegenName() const override;
        std::string toString() const override;
    };

    // define TQualifiedType<T>::NO_TYPE
    template<class T>
    TQualifiedType<T> TQualifiedType<T>::NO_TYPE = {};

    // convenience function
    bool typeIsVoid(std::shared_ptr<Type> type);
}
