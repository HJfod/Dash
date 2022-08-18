#pragma once

#include <utils/Types.hpp>
#include "Token.hpp"
#include <filesystem>
#include <compiler/Type.hpp>

namespace gdml::ast {
    using TypeCheckResult = LineResult<void>;
    using BranchInferResult = LineResult<Option<QualifiedType>>;

    namespace debug {
        #define GDML_DEBUG_FMT(class) \
            debug::indent(i) + #class "\n"

        #define GDML_DEBUG_FMT_PROP(prop) \
            debug::indent(i + debug::IND_LVL) + \
            "[" #prop ": " + std::to_string(prop) + "]\n"

        #define GDML_DEBUG_FMT_PROP_E(prop) \
            debug::indent(i + debug::IND_LVL) + \
            "[" #prop ": " + std::to_string(static_cast<int>(prop)) + "]\n"

        #define GDML_DEBUG_FMT_PROP_NUMT(prop) \
            debug::indent(i + debug::IND_LVL) + \
            "[" #prop ": " + dataTypeToString(prop) + "]\n"

        #define GDML_DEBUG_FMT_PROP_OP(prop) \
            debug::indent(i + debug::IND_LVL) + \
            "[" #prop ": " + tokenTypeToString(prop) + "]\n"

        #define GDML_DEBUG_FMT_PROP_S(prop) \
            debug::indent(i + debug::IND_LVL) + \
            "[" #prop ": " + prop + "]\n"

        #define GDML_DEBUG_FMT_PROP_FS(prop) \
            debug::indent(i + debug::IND_LVL) + \
            "[" #prop ": " + prop.string() + "]\n"

        #define GDML_DEBUG_FMT_CHILD_N(child, name) \
            debug::indent(i + debug::IND_LVL) + "(" name ")\n" + \
            child->debugPrintAST(i + debug::IND_LVL)

        #define GDML_DEBUG_FMT_CHILD(child) \
            GDML_DEBUG_FMT_CHILD_N(child, #child)

        #define GDML_DEBUG_FMT_CHILD_O(child) \
            (child.has_value() ?\
            GDML_DEBUG_FMT_CHILD_N(child.value(), #child) :\
            debug::indent(i + debug::IND_LVL) + "(" #child ": None)\n")
        
        #define GDML_TYPECHECK_CHILD(child) \
            {auto tch__ = child->compile(instance);if (!tch__) return tch__;}
        
        #define GDML_TYPECHECK_CHILD_O(child) \
            if (child.has_value()) { GDML_TYPECHECK_CHILD(child.value()); }
        
        #define GDML_TYPECHECK_CHILDREN(children) \
            for (auto& child : children) { GDML_TYPECHECK_CHILD(child); }

        static std::string indent(size_t amount) {
            return std::string(amount, ' ');
        }
        static constexpr const size_t IND_LVL = 4;
    }

    struct Stmt {
        SourceFile const* source;
        Position start;
        Position end;

        Stmt(
            SourceFile const* src,
            Position const& start,
            Position const& end
        ) : source(src),
            start(start),
            end(end) {}

        virtual Value* eval(Instance& instance) {
            return nullptr;
        }

        virtual ~Stmt() = default;
        virtual std::string debugPrintAST(size_t i) const = 0;
        virtual void codegen(Instance&, std::ostream& stream) const noexcept = 0;
        virtual TypeCheckResult compile(Instance&) noexcept {
            return Ok();
        };
        virtual BranchInferResult inferBranchReturnType(Instance& instance) {
            return Option<QualifiedType>(None);
        }
    };

    struct Expr : Stmt {
        QualifiedType evalType;

        Expr(
            SourceFile const* src,
            Position const& start,
            Position const& end
        ) : Stmt(src, start, end) {}
    };

    struct TypeExpr : Expr {
        types::TypeQualifiers qualifiers;

        TypeExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            types::TypeQualifiers const& qualifiers
        ) : Expr(src, start, end), qualifiers(qualifiers) {}

        std::string debugPrintASTTypeProps(size_t i) const {
            return 
                GDML_DEBUG_FMT_PROP(qualifiers.isConst);
        }
    };

    struct ValueExpr : Expr {
        ValueExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end
        ) : Expr(src, start, end) {}
    };

    struct LiteralExpr : ValueExpr {
        LiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end
        ) : ValueExpr(src, start, end) {}
    };

    // liberals

    struct BoolLiteralExpr : LiteralExpr {
        types::Bool value;

        BoolLiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            types::Bool value
        ) : LiteralExpr(src, start, end), value(value) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(BoolLiteralExpr) +
                GDML_DEBUG_FMT_PROP(value);
        }
        
        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance&) noexcept override;

        void codegen(Instance&, std::ostream& stream) const noexcept override {
            stream << (value ? "true" : "false");
        }
    };

    struct IntLiteralExpr : LiteralExpr {
        types::I64 value;
        types::DataType type;

        IntLiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            types::I64 value,
            types::DataType type
        ) : LiteralExpr(src, start, end),
            value(value), type(type) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(IntLiteralExpr) +
                GDML_DEBUG_FMT_PROP(value) + 
                GDML_DEBUG_FMT_PROP_NUMT(type);
        }

        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance&) noexcept override;

        void codegen(Instance&, std::ostream& stream) const noexcept override {
            if (type == types::DataType::I64) {
                stream << value << "ll";
            } else {
                stream << value;
            }
        }
    };

    struct UIntLiteralExpr : LiteralExpr {
        types::U64 value;
        types::DataType type;

        UIntLiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            types::U64 value,
            types::DataType type
        ) : LiteralExpr(src, start, end),
            value(value), type(type) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(UIntLiteralExpr) +
                GDML_DEBUG_FMT_PROP(value) + 
                GDML_DEBUG_FMT_PROP_NUMT(type);
        }

        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance&) noexcept override;

        void codegen(Instance&, std::ostream& stream) const noexcept override {
            if (type == types::DataType::U64) {
                stream << value << "ull";
            } else {
                stream << value << "u";
            }
        }
    };

    struct FloatLiteralExpr : LiteralExpr {
        types::F64 value;
        types::DataType type;

        FloatLiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            types::F64 value,
            types::DataType type
        ) : LiteralExpr(src, start, end),
            value(value), type(type) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(FloatLiteralExpr) +
                GDML_DEBUG_FMT_PROP(value) + 
                GDML_DEBUG_FMT_PROP_NUMT(type);
        }

        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance&) noexcept override;

        void codegen(Instance&, std::ostream& stream) const noexcept override {
            auto str = std::to_string(value);
            // make sure to add .0 at the end if the 
            // number doesn't have a decimal point in 
            // it (C++ doesn't accept 5f or 5d)
            if (str.find('.') == std::string::npos) {
                str += ".0";
            }
            stream << str;
            switch (type) {
                case types::DataType::F32: stream << "f";
                case types::DataType::F64: stream << "d";
            }
        }
    };

    struct StringLiteralExpr : LiteralExpr {
        types::String value;
        types::String rawValue;
    
        StringLiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            types::String const& value,
            types::String const& rawValue
        ) : LiteralExpr(src, start, end), value(value), rawValue(rawValue) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(StringLiteralExpr) +
                GDML_DEBUG_FMT_PROP_S(rawValue);
        }

        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance&) noexcept override;

        void codegen(Instance&, std::ostream& stream) const noexcept override {
            stream << "\"" << rawValue << "\"";
        }
    };

    struct InterpolatedLiteralExpr : LiteralExpr {
        // every other part of the string is a 
        // string, every other is an expression 
        // starting with string
        // like svsvsvsvsv...
        // also first and last component are always a string
        std::vector<types::String> strings;
        std::vector<types::String> rawStrings;
        std::vector<ValueExpr*> components;

        InterpolatedLiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::vector<types::String> const& strs,
            std::vector<types::String> const& rawstrs,
            std::vector<ValueExpr*> const& components
        ) : LiteralExpr(src, start, end), 
            strings(strs),
            rawStrings(rawstrs),
            components(components) {}

        std::string debugPrintAST(size_t i) const override {
            std::string str = GDML_DEBUG_FMT(InterpolatedLiteralExpr);
            // i think this should be guaranteed not to 
            // crash? if strings.size() ≈ components.size()
            for (size_t ix = 0; ix < strings.size() + components.size(); ix++) {
                if (ix % 2) {
                    str +=
                        debug::indent(i + debug::IND_LVL) + "(component)\n" +
                        components.at(ix / 2)->debugPrintAST(i + debug::IND_LVL);
                } else {
                    str +=
                        debug::indent(i + debug::IND_LVL) + "(string)\n" +
                        debug::indent(i + debug::IND_LVL) + "\"" + strings.at(ix / 2) + "\"\n";
                }
            }
            return str;
        }

        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance& instance) noexcept override;
    
        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    struct NoneLiteralExpr : LiteralExpr {
        NoneLiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end
        ) : LiteralExpr(src, start, end) {}

        std::string debugPrintAST(size_t i) const override {
            return GDML_DEBUG_FMT(NoneLiteralExpr);
        }

        void codegen(Instance&, std::ostream& stream) const noexcept override {
            stream << "std::nullopt";
        }
    };

    struct NullLiteralExpr : LiteralExpr {
        NullLiteralExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end
        ) : LiteralExpr(src, start, end) {}

        std::string debugPrintAST(size_t i) const override {
            return GDML_DEBUG_FMT(NullLiteralExpr);
        }

        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance&) noexcept override;

        void codegen(Instance&, std::ostream& stream) const noexcept override {
            stream << "nullptr";
        }
    };

    // values

    struct VariableDeclExpr : ValueExpr {
        Option<TypeExpr*> type;
        std::string name;
        Option<ValueExpr*> value;
        std::shared_ptr<Variable> variable = nullptr;

        VariableDeclExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Option<TypeExpr*> t,
            std::string const& n,
            Option<ValueExpr*> value
        ) : ValueExpr(src, start, end),
            type(t), name(n), value(value) {}
                
        std::string debugPrintAST(size_t i) const {
            return
                GDML_DEBUG_FMT(VariableDeclExpr) + 
                GDML_DEBUG_FMT_PROP_S(name) +
                GDML_DEBUG_FMT_CHILD_O(type) + 
                GDML_DEBUG_FMT_CHILD_O(value);
        }

        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance& instance) noexcept override;
    
        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    struct NameExpr : ValueExpr {
        std::string name;

        NameExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::string const& name
        ) : ValueExpr(src, start, end), name(name) {}

        virtual bool isScoped() const {
            return false;
        }

        virtual std::string fullName() const {
            return name;
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(NameExpr) +
                GDML_DEBUG_FMT_PROP_S(name);
        }

        void codegen(Instance&, std::ostream& stream) const noexcept override;
    };

    struct ScopeExpr : NameExpr {
        NameExpr* item;

        ScopeExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::string const& name,
            NameExpr* item
        ) : NameExpr(src, start, end, name), 
            item(item) {}

        bool isScoped() const override {
            return true;
        }
        
        std::string fullName() const override {
            return name + "::" + item->fullName();
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(ScopeExpr) +
                GDML_DEBUG_FMT_PROP_S(name) + 
                GDML_DEBUG_FMT_CHILD(item);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
    
        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    struct VariableExpr : ValueExpr {
        NameExpr* name;
        std::shared_ptr<Entity> entity = nullptr;

        VariableExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            NameExpr* name
        ) : ValueExpr(src, start, end), name(name) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(VariableExpr) +
                GDML_DEBUG_FMT_CHILD(name);
        }

        Value* eval(Instance& instance) override;
        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance&, std::ostream& stream) const noexcept override;
    };

    struct UnaryExpr : ValueExpr {
        enum Type : bool {
            Prefix,
            Suffix
        };

        TokenType op;
        ValueExpr* value;
        Type type;
    
        UnaryExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            TokenType op,
            ValueExpr* value,
            Type type
        ) : ValueExpr(src, start, end),
            op(op), value(value), type(type) {}

        TypeCheckResult compile(Instance& instance) noexcept override;
    
        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(UnaryExpr) +
                GDML_DEBUG_FMT_PROP_OP(op) + 
                GDML_DEBUG_FMT_PROP(type) + 
                GDML_DEBUG_FMT_CHILD(value);
        }

        void codegen(Instance& com, std::ostream& stream) const noexcept override {
            if (type == Prefix) {
                stream << tokenTypeToString(op);
            }
            value->codegen(com, stream);
            if (type == Suffix) {
                stream << tokenTypeToString(op);
            }
        }
    };

    struct BinaryExpr : ValueExpr {
        TokenType op;
        ValueExpr* LHS;
        ValueExpr* RHS;

        BinaryExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            TokenType op,
            ValueExpr* lhs,
            ValueExpr* rhs
        ) : ValueExpr(src, start, end),
            op(op), LHS(lhs), RHS(rhs) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(BinaryExpr) +
                GDML_DEBUG_FMT_PROP_OP(op) +
                GDML_DEBUG_FMT_CHILD(LHS) + 
                GDML_DEBUG_FMT_CHILD(RHS);
        }
        
        TypeCheckResult compile(Instance& instance) noexcept override;
    
        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    struct TernaryExpr : ValueExpr {
        ValueExpr* condition;
        ValueExpr* truthy;
        ValueExpr* falsy;

        TernaryExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            ValueExpr* condition,
            ValueExpr* truthy,
            ValueExpr* falsy
        ) : ValueExpr(src, start, end),
            condition(condition), truthy(truthy), falsy(falsy) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(TernaryExpr) +
                GDML_DEBUG_FMT_CHILD(condition) + 
                GDML_DEBUG_FMT_CHILD(truthy) +
                GDML_DEBUG_FMT_CHILD(falsy);
        }

        TypeCheckResult compile(Instance& instance) noexcept override {
            GDML_TYPECHECK_CHILD(condition);
            GDML_TYPECHECK_CHILD(truthy);
            GDML_TYPECHECK_CHILD(falsy);
            return Ok();
        }
    
        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    struct CallExpr : ValueExpr {
        ValueExpr* target;
        std::vector<ValueExpr*> args;

        CallExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            ValueExpr* target,
            std::vector<ValueExpr*> const& args
        ) : ValueExpr(src, start, end),
            target(target), args(args) {}

        std::string debugPrintAST(size_t i) const override {
            auto str = GDML_DEBUG_FMT(CallExpr);
            str += GDML_DEBUG_FMT_CHILD(target);
            for (auto& arg : args) {
                str += GDML_DEBUG_FMT_CHILD(arg);
            }
            return str;
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
    
        void codegen(Instance& com, std::ostream& stream) const noexcept override {
            stream << "(";
            target->codegen(com, stream);
            stream << ")(";
            bool firstArg = true;
            for (auto const& arg : args) {
                if (!firstArg) {
                    stream << ", ";
                }
                firstArg = false;
                arg->codegen(com, stream);
            }
            stream << ")";
        }
    };

    struct CastTypeExpr : ValueExpr {
        ValueExpr* target;
        TypeExpr* intoType;

        CastTypeExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            ValueExpr* target,
            TypeExpr* type
        ) : ValueExpr(src, start, end),
            target(target),
            intoType(type) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(CastTypeExpr) + 
                GDML_DEBUG_FMT_CHILD(target) + 
                GDML_DEBUG_FMT_CHILD(intoType);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
    
        void codegen(Instance&, std::ostream& stream) const noexcept override;
    };

    // types

    struct TypeNameExpr : TypeExpr {
        NameExpr* name;

        TypeNameExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            NameExpr* name,
            types::TypeQualifiers const& qualifiers
        ) : TypeExpr(src, start, end, qualifiers), name(name) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(TypeNameExpr) +
                GDML_DEBUG_FMT_CHILD(name) +
                debugPrintASTTypeProps(i);
        }
    
        void codegen(Instance&, std::ostream& stream) const noexcept override;
        TypeCheckResult compile(Instance&) noexcept override;
    };

    struct PointerExpr : TypeExpr {
        TypeExpr* to;
        types::PointerType type;

        PointerExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            TypeExpr* to,
            types::PointerType type,
            types::TypeQualifiers const& qualifiers
        ) : TypeExpr(src, start, end, qualifiers),
            to(to), type(type) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(PointerExpr) +
                GDML_DEBUG_FMT_CHILD(to) +
                GDML_DEBUG_FMT_PROP_E(type) +
                debugPrintASTTypeProps(i);
        }
        
        void codegen(Instance&, std::ostream& stream) const noexcept override;
        TypeCheckResult compile(Instance&) noexcept override;
    };

    struct TypeOfExpr : TypeExpr {
        Expr* value;

        TypeOfExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Expr* value
        ) : TypeExpr(src, start, end, types::TypeQualifiers()), value(value) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(TypeOfExpr) +
                GDML_DEBUG_FMT_CHILD(value);
        }

        TypeCheckResult compile(Instance& instance) noexcept override {
            GDML_TYPECHECK_CHILD(value);
            return Ok();
        }

        void codegen(Instance& com, std::ostream& stream) const noexcept override {
            stream << "typeof(";
            value->codegen(com, stream);
            stream << ")";
        }
    };

    struct FunctionTypeExpr : TypeExpr {
        std::vector<VariableDeclExpr*> parameters;
        Option<TypeExpr*> returnType;

        FunctionTypeExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::vector<VariableDeclExpr*> params,
            Option<TypeExpr*> rtype,
            types::TypeQualifiers const& qualifiers
        ) : TypeExpr(src, start, end, qualifiers),
            parameters(params),
            returnType(rtype) {}

        std::string debugPrintAST(size_t i) const override {
            auto pstring = GDML_DEBUG_FMT(FunctionTypeExpr);
            for (auto& param : parameters) {
                pstring += GDML_DEBUG_FMT_CHILD(param);
            }
            pstring += GDML_DEBUG_FMT_CHILD_O(returnType);
            pstring += debugPrintASTTypeProps(i);
            return pstring;
        }

        TypeCheckResult compile(Instance& instance) noexcept override;

        void codegen(Instance& com, std::ostream& stream) const noexcept override {
            if (returnType.has_value()) {
                returnType.value()->codegen(com, stream);
            } else {
                stream << "auto";
            }
            stream << "(*)(";
            bool firstArg = true;
            for (auto& param : parameters) {
                if (!firstArg) {
                    stream << ", ";
                }
                firstArg = false;
                param->codegen(com, stream);
            }
            stream << ")";
        }
    };

    struct ArrayTypeExpr : TypeExpr {
        TypeExpr* memberType;
        size_t capacity;

        ArrayTypeExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            TypeExpr* memberType,
            size_t capacity,
            types::TypeQualifiers const& qualifiers
        ) : TypeExpr(src, start, end, qualifiers),
            memberType(memberType),
            capacity(capacity) {}
        
        std::string debugPrintAST(size_t i) const override {
            return  
                GDML_DEBUG_FMT(ArrayTypeExpr) + 
                GDML_DEBUG_FMT_CHILD(memberType) +
                GDML_DEBUG_FMT_PROP(capacity);
        }

        TypeCheckResult compile(Instance& instance) noexcept override {
            GDML_TYPECHECK_CHILD(memberType);
            return Ok();
        }

        void codegen(Instance& com, std::ostream& stream) const noexcept override {
            if (capacity) {
                stream << "std::array<";
                memberType->codegen(com, stream);
                stream << ", " << capacity << ">";
            } else {
                stream << "std::vector<";
                memberType->codegen(com, stream);
                stream << ">";
            }
        }
    };

    // control flow

    struct StmtList : Stmt {
        std::vector<Stmt*> statements;

        StmtList(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::vector<Stmt*> const& stmnts
        ) : Stmt(src, start, end), statements(stmnts) {}

        std::string debugPrintAST(size_t i) const override {
            std::string str = GDML_DEBUG_FMT(StmtList);
            for (auto& expr : statements) {
                str += GDML_DEBUG_FMT_CHILD(expr);
            }
            return str;
        }

        TypeCheckResult compile(Instance& instance) noexcept override {
            GDML_TYPECHECK_CHILDREN(statements);
            return Ok();
        }
        BranchInferResult inferBranchReturnType(Instance& instance) override;

        void codegen(Instance& instance, std::ostream& stream) const noexcept override;
    };

    struct BlockStmt : Stmt {
        StmtList* body;

        BlockStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            StmtList* body
        ) : Stmt(src, start, end), body(body) {}

        std::string debugPrintAST(size_t i) const override {
            return 
                GDML_DEBUG_FMT(BlockStmt) +
                GDML_DEBUG_FMT_CHILD(body);
        }
        
        TypeCheckResult compile(Instance& instance) noexcept override;
        BranchInferResult inferBranchReturnType(Instance& instance) override;

        void codegen(Instance& instance, std::ostream& stream) const noexcept override;
    };

    struct IfStmt : Stmt {
        // if None, always truthy. used for else statements
        Option<ValueExpr*> condition;
        StmtList* branch;
        Option<IfStmt*> elseBranch;

        IfStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Option<ValueExpr*> condition,
            StmtList* branch,
            Option<IfStmt*> elseBranch
        ) : Stmt(src, start, end),
            condition(condition),
            branch(branch),
            elseBranch(elseBranch) {}
        
        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(IfStmt) +
                GDML_DEBUG_FMT_CHILD_O(condition) +
                GDML_DEBUG_FMT_CHILD(branch) + 
                GDML_DEBUG_FMT_CHILD_O(elseBranch);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        BranchInferResult inferBranchReturnType(Instance& instance) override;

        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    struct ReturnStmt : Stmt {
        ValueExpr* value;

        ReturnStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            ValueExpr* value
        ) : Stmt(src, start, end), value(value) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(ReturnStmt) +
                GDML_DEBUG_FMT_CHILD(value);
        }

        BranchInferResult inferBranchReturnType(Instance& instance) override;
        TypeCheckResult compile(Instance& instance) noexcept override {
            GDML_TYPECHECK_CHILD(value);
            return Ok();
        }

        void codegen(Instance& com, std::ostream& stream) const noexcept override {
            stream << "return ";
            value->codegen(com, stream);
        }
    };

    struct ClassDeclStmt : Stmt {
        std::string name;
        std::vector<VariableDeclExpr*> members;

        ClassDeclStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::string const& name,
            std::vector<VariableDeclExpr*> members
        ) : Stmt(src, start, end),
            name(name),
            members(members) {}

        std::string debugPrintAST(size_t i) const override {
            auto str = GDML_DEBUG_FMT(ClassDeclStmt);
            for (auto& member : members) {
                str += GDML_DEBUG_FMT_CHILD(member);
            }
            return str;
        }

        TypeCheckResult compile(Instance& instance) noexcept override {
            GDML_TYPECHECK_CHILDREN(members);
            return Ok();
        }

        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    struct NameSpaceStmt : Stmt {
        std::string name;
        StmtList* contents;

        NameSpaceStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::string const& name,
            StmtList* contents
        ) : Stmt(src, start, end),
            name(name), contents(contents) {}

        std::string debugPrintAST(size_t i) const override {
            return 
                GDML_DEBUG_FMT(NameSpaceStmt) +
                GDML_DEBUG_FMT_PROP_S(name) + 
                GDML_DEBUG_FMT_CHILD(contents);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;

        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    struct FunctionDeclStmt : Stmt {
        FunctionTypeExpr* type;
        NameExpr* name;
        Option<StmtList*> body;
        bool isImplementation;
        std::shared_ptr<FunctionEntity> entity = nullptr;

        FunctionDeclStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            FunctionTypeExpr* t,
            NameExpr* n,
            Option<StmtList*> body,
            bool impl
        ) : Stmt(src, start, end),
            type(t), name(n), body(body),
            isImplementation(impl) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(FunctionDeclStmt) + 
                GDML_DEBUG_FMT_CHILD(name) +
                GDML_DEBUG_FMT_CHILD(type) + 
                GDML_DEBUG_FMT_CHILD_O(body) + 
                GDML_DEBUG_FMT_PROP(isImplementation);
        }

        TypeCheckResult compile(Instance&) noexcept override;

        void codegen(Instance& com, std::ostream& stream) const noexcept override;
    };

    // embed

    struct EmbedCodeStmt : Stmt {
        std::string language;
        std::string data;

        EmbedCodeStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::string const& lang,
            std::string const& data
        ) : Stmt(src, start, end),
            language(lang), data(data) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(EmbedCodeStmt) + 
                GDML_DEBUG_FMT_PROP_S(language) + 
                GDML_DEBUG_FMT_PROP_S(data);
        }

        void codegen(Instance& instance, std::ostream& stream) const noexcept override;
    };

    // import / export

    struct ImportStmt : Stmt {
        std::filesystem::path path;
        bool isRelative;

        ImportStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            std::filesystem::path const& path,
            bool isRelative
        ) : Stmt(src, start, end),
            path(path), isRelative(isRelative) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(ImportStmt) +
                GDML_DEBUG_FMT_PROP_FS(path) + 
                GDML_DEBUG_FMT_PROP(isRelative);
        }

        void codegen(Instance& Instance, std::ostream& stream) const noexcept override;
    };

    // tree

    class AST {
    protected:
        std::vector<Stmt*> m_garbage;
        std::vector<Stmt*> m_tree;

    public:
        inline ~AST() {
            for (auto& g : m_garbage) {
                delete g;
            }
        }

        template<class S, typename... Args>
        S* make(Args... args) {
            static_assert(std::is_base_of_v<Stmt, S>, "AST::make requires an AST class");
            auto s = new S(std::forward<Args>(args)...);
            m_garbage.push_back(s);
            return s;
        }

        inline std::vector<Stmt*>& tree() {
            return m_tree;
        }

        void codegen(Instance& com, std::ostream& stream) const noexcept;

        inline TypeCheckResult compile(Instance& com) const noexcept {
            for (auto& stmt : m_tree) {
                auto res = stmt->compile(com);
                if (!res) return res;
            }
            return Ok();
        }
    };
}
