#pragma once

#include "Main.hpp"

namespace gdml::lang {
    struct Type;

    struct VoidType {};
    struct BoolType {};
    struct IntType {};
    struct FloatType {};
    struct StrType {};
    struct StructType {
        Option<std::string> name;
        Map<std::string, Type> members;
    };
    struct NodeType {
        std::string name;
        Map<std::string, Type> props;
    };

    struct Type {
        std::variant<
            VoidType,
            BoolType, IntType, FloatType, StrType,
            StructType, NodeType
        > kind;

        using Value = decltype(kind);

        Type() = default;
        Type(Value const& value) : kind(value) {}

        bool operator==(Type const& other) const;
        std::string toString() const;
    };

    struct Var final {
        std::string name;
        Type type;
    };

    struct Scope final {
        Map<std::string, Type> types;
        Map<std::string, Var> vars;
    };

    class State final {
    private:
        Vec<Scope> m_scopes;

    public:
        State();

        void pushType(Type const& type);
        Type* getType(std::string const& name, bool topOnly = false);

        void pushVar(Var const& var);
        Var* getVar(std::string const& name, bool topOnly = false);

        void pushScope();
        void popScope();
    };
}
