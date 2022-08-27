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

        #define GDML_DEBUG_FMT_R(text) \
            debug::indent(i + debug::IND_LVL) + text + "\n"

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
        
        #define GDML_TYPECHECK_CHILDREN_M(children) \
            for (auto& [_, child] : children) { GDML_TYPECHECK_CHILD(child); }
        
        #define GDML_APPLY_PARENT(child) \
            child->parent = this
            
        #define GDML_APPLY_PARENT_O(child) \
            if (child.has_value()) GDML_APPLY_PARENT(child.value())
            
        #define GDML_APPLY_PARENTS(children) \
            for (auto& child : children) GDML_APPLY_PARENT(child)
            
        #define GDML_APPLY_PARENTS_M(children) \
            for (auto& [_, child] : children) GDML_APPLY_PARENT(child)
        
        #define GDML_SWAP_CHILD(child) \
            if (child == static_cast<decltype(child)>(stmt)) {\
                child = static_cast<decltype(child)>(to);\
                to->parent = this;\
                return true;\
            }
        
        #define GDML_SWAP_CHILD_O(child) \
            if (child.has_value() && \
                child.value() == static_cast<decltype(child)::value_type>(stmt)) {\
                child.value() = static_cast<decltype(child)::value_type>(to);\
                to->parent = this;\
                return true;\
            }
        
        #define GDML_SWAP_CHILDREN(children) \
            if (replaceOneInVector(\
                children,\
                static_cast<decltype(children)::value_type>(stmt),\
                static_cast<decltype(children)::value_type>(to)\
            )) {\
                to->parent = this;\
                return true;\
            }
        
        #define GDML_SWAP_CHILDREN_M(children) \
            if (replaceOneInMap(\
                children,\
                static_cast<decltype(children)::mapped_type>(stmt),\
                static_cast<decltype(children)::mapped_type>(to)\
            )) {\
                to->parent = this;\
                return true;\
            }
            

        static std::string indent(size_t amount) {
            return std::string(amount, ' ');
        }
        static constexpr const size_t IND_LVL = 4;
    }

    struct Stmt {
        SourceFile const* source;
        Position start;
        Position end;
        Stmt* parent = nullptr;
        ast::AST* ast;

        Stmt(
            SourceFile const* src,
            Position const& start,
            Position const& end
        ) : source(src),
            start(start),
            end(end),
            parent(parent) {}

        virtual std::shared_ptr<Value> eval(Instance& instance) {
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
        
        // Get the current parent in the nearest
        // parent statement list
        virtual Stmt* getParentStatement(Stmt* current = nullptr) {
            return parent ? parent->getParentStatement(this) : nullptr;
        }

        // Insert a statment into the nearest 
        // parent statement list
        virtual bool insertStatement(
            Stmt* stmt,
            Option<Stmt*> const& relative,
            bool after
        ) {
            return parent ? parent->insertStatement(
                stmt, (relative ? Some(this) : None), after
            ) : false;
        }
       
        // Remove statement from a statement list 
        // (if it is in a position where it can be 
        // removed from). Make sure that the 
        // statement's children have been put up for 
        // adoption to other AST nodes or otherwise 
        // a child may end up depending on a parent 
        // that's no longer there
        virtual bool removeStatement(Stmt* stmt) {
            return parent ? parent->removeStatement(stmt) : false;
        }
        
        // Replaces a statement in the parent with 
        // another. note that the parent might not 
        // perform type-checks; however, if for 
        // example statements need to be swapped 
        // due to compile-time calculations, the 
        // statement can be sure that its parent 
        // accepts its own type by virtue of the 
        // statement being there. In addition, 
        // because nearly all statements that 
        // support swapping either take Stmt, 
        // TypeExpr or ValueExpr as children, one 
        // can be near-certain that if it swaps 
        // to another one of those that the swap 
        // will be succesful. If the parent takes 
        // a more specialized type, then it will 
        // likely perform a type-check.
        virtual bool swap(Stmt* stmt, Stmt* to) {
            return false;
        }
    };

    struct Expr : Stmt {
        QualifiedType evalType;
        bool usedAsStmt = false;

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
        
        std::shared_ptr<Value> eval(Instance& instance) override;
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

        std::shared_ptr<Value> eval(Instance& instance) override;
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

        std::shared_ptr<Value> eval(Instance& instance) override;
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

        std::shared_ptr<Value> eval(Instance& instance) override;
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
                case types::DataType::F32: stream << "f"; break;
                case types::DataType::F64: stream << "d"; break;
                default: break;
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

        std::shared_ptr<Value> eval(Instance& instance) override;
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
            components(components)
        {
            GDML_APPLY_PARENTS(components);
        }

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

        std::shared_ptr<Value> eval(Instance& instance) override;
        TypeCheckResult compile(Instance& instance) noexcept override;
    
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;
        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILDREN(components);
            return false;
        }
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

        std::shared_ptr<Value> eval(Instance& instance) override;
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

    protected:
        [[nodiscard]]
        TypeCheckResult inferType(Instance& instance, bool isMember) noexcept;

    public:

        VariableDeclExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Option<TypeExpr*> t,
            std::string const& n,
            Option<ValueExpr*> value
        ) : ValueExpr(src, start, end),
            type(t), name(n), value(value)
        {
            GDML_APPLY_PARENT_O(type);
            GDML_APPLY_PARENT_O(value);
        }
                
        std::string debugPrintAST(size_t i) const {
            return
                GDML_DEBUG_FMT(VariableDeclExpr) + 
                GDML_DEBUG_FMT_PROP_S(name) +
                GDML_DEBUG_FMT_CHILD_O(type) + 
                GDML_DEBUG_FMT_CHILD_O(value);
        }

        std::shared_ptr<Value> eval(Instance& instance) override;
        TypeCheckResult compile(Instance& instance) noexcept override;
        TypeCheckResult compile(std::shared_ptr<Class> entity, Instance& instance) noexcept;
        TypeCheckResult compileAsMember(std::shared_ptr<Class> entity, Instance& instance) noexcept;
    
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;
        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD_O(type);
            GDML_SWAP_CHILD_O(value);
            return false;
        }
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

        virtual std::vector<std::string> fullNameList() const {
            return { name };
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
            item(item)
        {
            GDML_APPLY_PARENT(item);
        }

        bool isScoped() const override {
            return true;
        }
        
        std::string fullName() const override {
            return name + "::" + item->fullName();
        }

        std::vector<std::string> fullNameList() const override {
            std::vector<std::string> vec { name };
            auto ivec = item->fullNameList();
            vec.insert(vec.end(), ivec.begin(), ivec.end());
            return vec;
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(ScopeExpr) +
                GDML_DEBUG_FMT_PROP_S(name) + 
                GDML_DEBUG_FMT_CHILD(item);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
    
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(item);
            return false;
        }
    };

    struct MemberExpr : ValueExpr {
        enum Kind {
            Object,  // .
            Pointer, // ->
            Optional,  // ?->
        } kind;
        ValueExpr* object;
        NameExpr* member;
        QualifiedType objectType;

        static Kind tokenTypeToKind(TokenType op);

        MemberExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Kind kind,
            ValueExpr* object,
            NameExpr* member
        ) : ValueExpr(src, start, end),
            kind(kind), object(object), member(member)
        {
            GDML_APPLY_PARENT(object);
            GDML_APPLY_PARENT(member);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(MemberExpr) +
                GDML_DEBUG_FMT_CHILD(object) + 
                GDML_DEBUG_FMT_CHILD(member);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        TypeCheckResult compileWithParams(
            Instance& instance, 
            Option<std::vector<Parameter>> const& args
        ) noexcept;
        std::shared_ptr<Value> eval(Instance& instance) override;
        void codegen(Instance&, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(object);
            GDML_SWAP_CHILD(member);
            return false;
        }
    };

    struct VariableExpr : ValueExpr {
        NameExpr* name;
        std::shared_ptr<Entity> entity = nullptr;

        VariableExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            NameExpr* name
        ) : ValueExpr(src, start, end), name(name) {
            GDML_APPLY_PARENT(name);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(VariableExpr) +
                GDML_DEBUG_FMT_CHILD(name);
        }

        std::shared_ptr<Value> eval(Instance& instance) override;
        TypeCheckResult compile(Instance& instance) noexcept override;
        TypeCheckResult compileWithParams(
            Instance& instance, 
            Option<std::vector<Parameter>> const& args
        ) noexcept;
        void codegen(Instance&, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(name);
            return false;
        }
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
            op(op), value(value), type(type)
        {
            GDML_APPLY_PARENT(value);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
    
        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(UnaryExpr) +
                GDML_DEBUG_FMT_PROP_OP(op) + 
                GDML_DEBUG_FMT_PROP(type) + 
                GDML_DEBUG_FMT_CHILD(value);
        }

        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(value);
            return false;
        }
    };

    struct BinaryExpr : ValueExpr {
        TokenType op;
        ValueExpr* LHS;
        ValueExpr* RHS;
        ImplStatus status;

        BinaryExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            TokenType op,
            ValueExpr* lhs,
            ValueExpr* rhs
        ) : ValueExpr(src, start, end),
            op(op), LHS(lhs), RHS(rhs)
        {
            GDML_APPLY_PARENT(LHS);
            GDML_APPLY_PARENT(RHS);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(BinaryExpr) +
                GDML_DEBUG_FMT_PROP_OP(op) +
                GDML_DEBUG_FMT_CHILD(LHS) + 
                GDML_DEBUG_FMT_CHILD(RHS);
        }
        
        std::shared_ptr<Value> eval(Instance& instance) override;
        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(LHS);
            GDML_SWAP_CHILD(RHS);
            return false;
        }
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
            condition(condition), truthy(truthy), falsy(falsy)
        {
            GDML_APPLY_PARENT(condition);
            GDML_APPLY_PARENT(truthy);
            GDML_APPLY_PARENT(falsy);
        }

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
    
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(condition);
            GDML_SWAP_CHILD(truthy);
            GDML_SWAP_CHILD(falsy);
            return false;
        }
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
            target(target), args(args)
        {
            GDML_APPLY_PARENT(target);
            GDML_APPLY_PARENTS(args);
        }

        std::string debugPrintAST(size_t i) const override {
            auto str = GDML_DEBUG_FMT(CallExpr);
            str += GDML_DEBUG_FMT_CHILD(target);
            for (auto& arg : args) {
                str += GDML_DEBUG_FMT_CHILD(arg);
            }
            return str;
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(target);
            GDML_SWAP_CHILDREN(args);
            return false;
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
            intoType(type)
        {
            GDML_APPLY_PARENT(target);
            GDML_APPLY_PARENT(intoType);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(CastTypeExpr) + 
                GDML_DEBUG_FMT_CHILD(target) + 
                GDML_DEBUG_FMT_CHILD(intoType);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance&, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(target);
            GDML_SWAP_CHILD(intoType);
            return false;
        }
    };

    struct ConstructExpr : ValueExpr {
        Option<NameExpr*> name;
        bool isNew;
        std::vector<ValueExpr*> values;
        std::unordered_map<std::string, ValueExpr*> namedValues;
        std::shared_ptr<Class> classEntity = nullptr;
        std::shared_ptr<FunctionEntity> funEntity = nullptr;

        ConstructExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Option<NameExpr*> const& name,
            bool isNew,
            std::vector<ValueExpr*> const& values,
            std::unordered_map<std::string, ValueExpr*> const& namedValues
        ) : ValueExpr(src, start, end),
            name(name), isNew(isNew), values(values),
            namedValues(namedValues)
        {
            GDML_APPLY_PARENT_O(name);
            GDML_APPLY_PARENTS(values);
            GDML_APPLY_PARENTS_M(namedValues);
        }

        std::string debugPrintAST(size_t i) const override {
            auto str = GDML_DEBUG_FMT(ConstructExpr) +
                GDML_DEBUG_FMT_CHILD_O(name) + 
                GDML_DEBUG_FMT_PROP(isNew);

            str += GDML_DEBUG_FMT_R("(values)");
            i += debug::IND_LVL;
            for (auto& expr : values) {
                str += GDML_DEBUG_FMT_CHILD(expr);
            }
            i -= debug::IND_LVL;

            str += GDML_DEBUG_FMT_R("(namedValues)");
            i += debug::IND_LVL;
            for (auto& [name, val] : namedValues) {
                str += GDML_DEBUG_FMT_R("name: " + name);
                str += GDML_DEBUG_FMT_CHILD(val);
            }
            i -= debug::IND_LVL;

            return str;
        }

        TypeCheckResult checkInitializer(std::shared_ptr<Class> entity, Instance& instance) noexcept;
        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD_O(name);
            GDML_SWAP_CHILDREN(values);
            GDML_SWAP_CHILDREN_M(namedValues);
            return false;
        }
    };

    struct PointerToExpr : ValueExpr {
        ValueExpr* value;
        bool deref;

        PointerToExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            ValueExpr* value,
            bool deref
        ) : ValueExpr(src, start, end), value(value), deref(deref) {
            GDML_APPLY_PARENT(value);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(PointerToExpr) +
                GDML_DEBUG_FMT_CHILD(value) + 
                GDML_DEBUG_FMT_PROP(deref);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance&, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(value);
            return false;
        }
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
        ) : TypeExpr(src, start, end, qualifiers), name(name) {
            GDML_APPLY_PARENT(name);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(TypeNameExpr) +
                GDML_DEBUG_FMT_CHILD(name) +
                debugPrintASTTypeProps(i);
        }
    
        void codegen(Instance&, std::ostream& stream) const noexcept override;
        TypeCheckResult compile(Instance&) noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(name);
            return false;
        }
    };

    struct PointerExpr : TypeExpr {
        TypeExpr* to;

        PointerExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            TypeExpr* to,
            types::TypeQualifiers const& qualifiers
        ) : TypeExpr(src, start, end, qualifiers),
            to(to)
        {
            GDML_APPLY_PARENT(to);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(PointerExpr) +
                GDML_DEBUG_FMT_CHILD(to) +
                debugPrintASTTypeProps(i);
        }
        
        void codegen(Instance&, std::ostream& stream) const noexcept override;
        TypeCheckResult compile(Instance&) noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(to);
            return false;
        }
    };

    struct TypeOfExpr : TypeExpr {
        Expr* value;

        TypeOfExpr(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Expr* value
        ) : TypeExpr(src, start, end, types::TypeQualifiers()),
            value(value)
        {
            GDML_APPLY_PARENT(value);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(TypeOfExpr) +
                GDML_DEBUG_FMT_CHILD(value);
        }

        TypeCheckResult compile(Instance& instance) noexcept override {
            GDML_TYPECHECK_CHILD(value);
            return Ok();
        }

        void codegen(Instance& instance, std::ostream& stream) const noexcept override {
            stream << "typeof(";
            value->codegen(instance, stream);
            stream << ")";
        }

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(value);
            return false;
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
            returnType(rtype)
        {
            GDML_APPLY_PARENTS(parameters);
            GDML_APPLY_PARENT_O(returnType);
        }

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
        TypeCheckResult compileAsMember(
            std::shared_ptr<Class> entity,
            Instance& instance
        ) noexcept;

        void codegen(Instance& instance, std::ostream& stream) const noexcept override {
            if (returnType.has_value()) {
                returnType.value()->codegen(instance, stream);
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
                param->codegen(instance, stream);
            }
            stream << ")";
        }

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILDREN(parameters);
            GDML_SWAP_CHILD_O(returnType);
            return false;
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
            capacity(capacity)
        {
            GDML_APPLY_PARENT(memberType);
        }
        
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

        void codegen(Instance& instance, std::ostream& stream) const noexcept override {
            if (capacity) {
                stream << "std::array<";
                memberType->codegen(instance, stream);
                stream << ", " << capacity << ">";
            } else {
                stream << "std::vector<";
                memberType->codegen(instance, stream);
                stream << ">";
            }
        }

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(memberType);
            return false;
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
        ) : Stmt(src, start, end),
            statements(stmnts)
        {
            GDML_APPLY_PARENTS(statements);
        }

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

        Stmt* getParentStatement(Stmt* current = nullptr) override;
        bool insertStatement(
            Stmt* stmt,
            Option<Stmt*> const& relative,
            bool after
        ) override;
        bool removeStatement(Stmt* stmt) override;
        bool swap(Stmt* stmt, Stmt* to) override;
    };

    struct BlockStmt : Stmt {
        StmtList* body;

        BlockStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            StmtList* body
        ) : Stmt(src, start, end),
            body(body)
        {
            GDML_APPLY_PARENT(body);
        }

        std::string debugPrintAST(size_t i) const override {
            return 
                GDML_DEBUG_FMT(BlockStmt) +
                GDML_DEBUG_FMT_CHILD(body);
        }
        
        TypeCheckResult compile(Instance& instance) noexcept override;
        BranchInferResult inferBranchReturnType(Instance& instance) override;

        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(body);
            return false;
        }
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
            elseBranch(elseBranch)
        {
            GDML_APPLY_PARENT_O(condition);
            GDML_APPLY_PARENT(branch);
            GDML_APPLY_PARENT_O(elseBranch);
        }
        
        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(IfStmt) +
                GDML_DEBUG_FMT_CHILD_O(condition) +
                GDML_DEBUG_FMT_CHILD(branch) + 
                GDML_DEBUG_FMT_CHILD_O(elseBranch);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        BranchInferResult inferBranchReturnType(Instance& instance) override;

        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD_O(condition);
            GDML_SWAP_CHILD(branch);
            GDML_SWAP_CHILD_O(elseBranch);
            return false;
        }
    };

    struct ReturnStmt : Stmt {
        ValueExpr* value;

        ReturnStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            ValueExpr* value
        ) : Stmt(src, start, end),
            value(value)
        {
            GDML_APPLY_PARENT(value);
        }

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

        void codegen(Instance& instance, std::ostream& stream) const noexcept override {
            stream << "return ";
            value->codegen(instance, stream);
        }

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(value);
            return false;
        }
    };

    struct NameSpaceStmt : Stmt {
        Option<NameExpr*> name;
        StmtList* content;

        NameSpaceStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Option<NameExpr*> name,
            StmtList* content
        ) : Stmt(src, start, end),
            name(name), content(content)
        {
            GDML_APPLY_PARENT_O(name);
            GDML_APPLY_PARENT(content);
        }

        std::string debugPrintAST(size_t i) const override {
            return 
                GDML_DEBUG_FMT(NameSpaceStmt) +
                GDML_DEBUG_FMT_CHILD_O(name) + 
                GDML_DEBUG_FMT_CHILD(content);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD_O(name);
            GDML_SWAP_CHILD(content);
            return false;
        }
    };

    struct AFunctionDeclStmt : Stmt {
        Option<StmtList*> body;
        std::shared_ptr<Class> parentClass = nullptr;
        std::shared_ptr<FunctionEntity> entity = nullptr;

        AFunctionDeclStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            Option<StmtList*> body
        ) : Stmt(src, start, end),
            body(body)
        {
            GDML_APPLY_PARENT_O(body);
        }
    };

    struct FunctionDeclStmt : AFunctionDeclStmt {
        FunctionTypeExpr* type;
        NameExpr* name;
        bool isImplementation;
    
    protected:
        TypeCheckResult inferReturnType(Instance& instance);

    public:

        FunctionDeclStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            FunctionTypeExpr* type,
            NameExpr* n,
            Option<StmtList*> body,
            bool impl
        ) : AFunctionDeclStmt(src, start, end, body),
            type(type), name(n), 
            isImplementation(impl)
        {
            GDML_APPLY_PARENT(type);
            GDML_APPLY_PARENT(name);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(FunctionDeclStmt) + 
                GDML_DEBUG_FMT_CHILD(name) +
                GDML_DEBUG_FMT_CHILD(type) + 
                GDML_DEBUG_FMT_CHILD_O(body) + 
                GDML_DEBUG_FMT_PROP(isImplementation);
        }

        TypeCheckResult compile(Instance&) noexcept override;
        TypeCheckResult compileAsMember(std::shared_ptr<Class> entity, Instance& instance) noexcept;

        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(type);
            GDML_SWAP_CHILD(name);
            GDML_SWAP_CHILD_O(body);
            return false;
        }
    };

    struct ConstructorDeclStmt : AFunctionDeclStmt {
        bool isDestructor;
        std::vector<VariableDeclExpr*> parameters;
        bool isDefault;

        ConstructorDeclStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            bool isDestructor,
            std::vector<VariableDeclExpr*> const& parameters,
            Option<StmtList*> const& body,
            bool isDefault
        ) : AFunctionDeclStmt(src, start, end, body),
            isDestructor(isDestructor),
            parameters(parameters),
            isDefault(isDefault)
        {
            GDML_APPLY_PARENTS(parameters);
            GDML_APPLY_PARENT_O(body);
        }

        std::string debugPrintAST(size_t i) const override {
            auto str = 
                GDML_DEBUG_FMT(ConstructorDeclStmt) + 
                GDML_DEBUG_FMT_PROP(isDestructor);
            for (auto& param : parameters) {
                str += GDML_DEBUG_FMT_CHILD(param);
            }
            str +=
                GDML_DEBUG_FMT_CHILD_O(body) + 
                GDML_DEBUG_FMT_PROP(isDefault);
            return str;
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        TypeCheckResult compile(std::shared_ptr<Class> entity, Instance& instance) noexcept;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILDREN(parameters);
            GDML_SWAP_CHILD_O(body);
            return false;
        }
    };

    struct ClassFwdDeclStmt : Stmt {
        NameExpr* name;
        bool isStruct;

        ClassFwdDeclStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            NameExpr* name,
            bool isStruct
        ) : Stmt(src, start, end),
            name(name), isStruct(isStruct)
        {
            GDML_APPLY_PARENT(name);
        }

        std::string debugPrintAST(size_t i) const override {
            return GDML_DEBUG_FMT(ClassFwdDeclStmt) + 
                GDML_DEBUG_FMT_CHILD(name) + 
                GDML_DEBUG_FMT_PROP(isStruct);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(name);
            return false;
        }
    };

    struct ClassDeclStmt : Stmt {
        NameExpr* name;
        std::vector<VariableDeclExpr*> members;
        std::vector<FunctionDeclStmt*> functions;
        std::vector<ConstructorDeclStmt*> constructors;
        bool isStruct;
        std::shared_ptr<Class> entity;

        ClassDeclStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            NameExpr* name,
            std::vector<VariableDeclExpr*> const& members,
            std::vector<FunctionDeclStmt*> const& functions,
            std::vector<ConstructorDeclStmt*> const& constructors,
            bool isStruct
        ) : Stmt(src, start, end),
            name(name),
            members(members),
            functions(functions),
            constructors(constructors),
            isStruct(isStruct)
        {
            GDML_APPLY_PARENT(name);
            GDML_APPLY_PARENTS(members);
            GDML_APPLY_PARENTS(functions);
            GDML_APPLY_PARENTS(constructors);
        }

        std::string debugPrintAST(size_t i) const override {
            auto str = GDML_DEBUG_FMT(ClassDeclStmt) + 
                GDML_DEBUG_FMT_CHILD(name) + 
                GDML_DEBUG_FMT_PROP(isStruct);
            for (auto& member : members) {
                str += GDML_DEBUG_FMT_CHILD(member);
            }
            for (auto& function : functions) {
                str += GDML_DEBUG_FMT_CHILD(function);
            }
            for (auto& constructor : constructors) {
                str += GDML_DEBUG_FMT_CHILD(constructor);
            }
            return str;
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(name);
            GDML_SWAP_CHILDREN(members);
            GDML_SWAP_CHILDREN(functions);
            GDML_SWAP_CHILDREN(constructors);
            return false;
        }
    };

    struct UsingNameSpaceStmt : Stmt {
        NameExpr* name;

        UsingNameSpaceStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            NameExpr* name
        ) : Stmt(src, start, end),
            name(name)
        {
            GDML_APPLY_PARENT(name);
        }

        std::string debugPrintAST(size_t i) const override {
            return 
                GDML_DEBUG_FMT(UsingNameSpaceStmt) +
                GDML_DEBUG_FMT_CHILD(name);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(name);
            return false;
        }
    };

    // embed

    struct ExternStmt : Stmt {
        StmtList* content;

        ExternStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            StmtList* content
        ) : Stmt(src, start, end),
            content(content)
        {
            GDML_APPLY_PARENT(content);
        }

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(ExternStmt) + 
                GDML_DEBUG_FMT_CHILD(content);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& instance, std::ostream& stream) const noexcept override;

        bool swap(Stmt* stmt, Stmt* to) override {
            GDML_SWAP_CHILD(content);
            return false;
        }
    };

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

    // debug

    // todo: get rid of this after macros have been implemented and use those
    struct DebugStmt : Stmt {
        TokenType token;
        
        DebugStmt(
            SourceFile const* src,
            Position const& start,
            Position const& end,
            TokenType token
        ) : Stmt(src, start, end),
            token(token) {}

        std::string debugPrintAST(size_t i) const override {
            return
                GDML_DEBUG_FMT(DebugStmt) +
                GDML_DEBUG_FMT_PROP_OP(token);
        }

        TypeCheckResult compile(Instance& instance) noexcept override;
        void codegen(Instance& Instance, std::ostream& stream) const noexcept override;
    };

    // tree

    class AST : public StmtList {
    protected:
        std::vector<Stmt*> m_garbage;

    public:
        AST(
            SourceFile const* src,
            Position const& start,
            Position const& end
        );
        virtual ~AST();

        template<class S, typename... Args>
        S* make(Args... args) {
            static_assert(std::is_base_of_v<Stmt, S>, "AST::make requires an AST class");
            auto s = new S(std::forward<Args>(args)...);
            s->ast = this;
            m_garbage.push_back(s);
            return s;
        }

        inline std::vector<Stmt*>& tree() {
            return statements;
        }
    };
}
