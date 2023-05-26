#pragma once

#include "Main.hpp"
#include "Token.hpp"

namespace gdml::lang {
    struct Type;
    struct Value;
    class Expr;

    struct GDML_DLL IdentPath {
        Ident name;
        Vec<Ident> path;
        bool absolute = false;

        IdentPath();
        IdentPath(
            Vec<Ident> const& path, bool absolute,
            std::source_location const loc = std::source_location::current()
        );
        explicit IdentPath(Ident const& name);

        bool operator==(IdentPath const& other) const;
        std::string toString() const;
        bool isSingle() const;
        Vec<Ident> getComponents() const;
        Option<IdentPath> getParent() const;

        // Even if the other path is absolute, it is still joined at the end
        IdentPath joinForce(IdentPath const& other) const;
    };

    struct GDML_DLL FullIdentPath {
        Vec<Ident> path;

        FullIdentPath() = default;
        FullIdentPath(Vec<Ident> const& components);
        explicit FullIdentPath(IdentPath const& path);

        bool operator==(FullIdentPath const& other) const;
        std::string toString() const;

        FullIdentPath join(Ident const& component) const;
        FullIdentPath join(IdentPath const& components) const;
        // If the IdentPath is absolute, the resulting FullIdentPath will be just 
        // that IdentPath, otherwise it is appended to the end of the current 
        // FullIdentPath
        FullIdentPath enter(IdentPath const& components) const;
    };

    struct GDML_DLL UnkType {};
    struct GDML_DLL VoidType {};
    struct GDML_DLL BoolType {};
    struct GDML_DLL IntType {};
    struct GDML_DLL FloatType {};
    struct GDML_DLL StrType {};

    struct GDML_DLL ParamType {
        Ident name;
        Box<Type> type; // may be unknown - remember to check at call site that function body is valid!!!
    };

    struct GDML_DLL FunType {
        Option<IdentPath> name;
        Vec<ParamType> params;
        Option<Box<Type>> retType;
        bool isExtern;
    };

    struct GDML_DLL PropType {
        Box<Type> type;
        Vec<Ident> dependencies;
        bool required;

        bool operator==(PropType const&) const = default;
    };

    struct GDML_DLL StructType {
        Option<IdentPath> name;
        Map<Ident, PropType> members;
        Option<Box<Type>> super;
        bool isExtern;
    };
    
    struct GDML_DLL NodeType {
        IdentPath name;
        Map<Ident, PropType> props;
        Option<Box<Type>> super;
    };

    struct GDML_DLL EnumType {
        Option<IdentPath> name;
        Map<Ident, Type> variants;
        Option<Box<Type>> super;
        bool isExtern;
    };

    struct GDML_DLL RefType {
        Box<Type> type;
    };

    struct GDML_DLL AliasType {
        IdentPath alias;
        Box<Type> type;
    };

    enum class Primitive {
        Unk,
        Void,
        Bool,
        Int,
        Float,
        Str,
    };

    class GDML_DLL Type final {
    private:
        std::variant<
            UnkType,
            VoidType, BoolType, IntType, FloatType, StrType,
            FunType,
            StructType, NodeType, EnumType,
            RefType, AliasType
        > kind;
        Rc<const Expr> decl;

    public:
        using Value = decltype(kind);

        Type() = default;
        Type(Value const& value, Rc<const Expr> decl);
        Type(Primitive type, std::source_location const loc = std::source_location::current());

        Type realize() const;
        Option<Type> getReturnType() const;
        bool convertible(Type const& other) const;
        Option<Type> getMemberType(std::string const& name) const;
        Set<String> getRequiredMembers() const;
        std::string toString() const;
        Option<IdentPath> getName() const;
        Rc<const Expr> getDecl() const;
        bool isExportable() const;

        template <class T>
        bool has() const {
            return std::holds_alternative<T>(this->kind);
        }

        template <class T>
        T get(std::source_location const loc = std::source_location::current()) const {
            try {
                return std::get<T>(this->kind);
            }
            catch(std::exception const& e) {
                throw std::runtime_error(fmt::format(
                    "Attempted to get an invalid Type (holding index {}) (from {})",
                    this->kind.index(), loc
                ));
            }
        }
    };

    GDML_DLL std::string opFunName(Op op, Type a);
    GDML_DLL std::string opFunName(Op op, Type a, Type b);
    GDML_DLL std::string asFunName(Type a, Type b);

    struct GDML_DLL PropValue {
        Box<Value> value;
    };

    struct GDML_DLL StructValue {
        StructType type;
        Map<std::string, PropValue> members;
    };

    struct GDML_DLL NodeValue {
        NodeType type;
        Map<std::string, PropValue> props;
    };

    struct GDML_DLL RefValue {
        RefType type;
        Rc<Value> value;
    };

    struct GDML_DLL Value {
        std::variant<
            VoidLit, BoolLit, IntLit, FloatLit, StrLit,
            StructValue, NodeValue, RefValue
        > kind;

        Type getType() const;
    };
    
    using TypeCheckResult = ParseResult<Type>;
}

template <>
struct std::hash<gdml::lang::IdentPath> {
    std::size_t operator()(gdml::lang::IdentPath const& path) const noexcept;
};

template <>
struct std::hash<gdml::lang::FullIdentPath> {
    std::size_t operator()(gdml::lang::FullIdentPath const& path) const noexcept;
};
