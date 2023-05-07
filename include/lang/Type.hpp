#pragma once

#include "Main.hpp"
#include "Token.hpp"

namespace gdml::lang {
    struct Type;
    struct Value;

    struct GDML_DLL UnkType {};
    struct GDML_DLL VoidType {};
    struct GDML_DLL BoolType {};
    struct GDML_DLL IntType {};
    struct GDML_DLL FloatType {};
    struct GDML_DLL StrType {};

    struct GDML_DLL PropType {
        Box<Type> type;
        Option<Box<Value>> defaultValue;
        Vec<Ident> dependencies;
        bool opaque; // has getter and setter

        bool operator==(PropType const&) const = default;
    };

    struct GDML_DLL StructType {
        Option<std::string> name;
        Map<std::string, PropType> members;
    };
    struct GDML_DLL NodeType {
        std::string name;
        Map<std::string, PropType> props;
    };
    struct GDML_DLL RefType {
        Box<Type> type;
    };

    struct GDML_DLL Type {
        std::variant<
            UnkType,
            VoidType,
            BoolType, IntType, FloatType, StrType,
            StructType, NodeType, RefType
        > kind;

        using Value = decltype(kind);

        Type() = default;
        Type(Value const& value) : kind(value) {}

        bool equal(Type const& other) const;
        bool convertible(Type const& other) const;
        Option<Type> getMemberType(std::string const& name) const;
        Set<String> getRequiredMembers() const;
        std::string toString() const;
        bool isExportable() const;
    };

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
