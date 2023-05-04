#pragma once

#include "Main.hpp"
#include "Token.hpp"

namespace gdml::lang {
    struct Type;
    struct Value;

    struct GDML_DLL VoidType {};
    struct GDML_DLL BoolType {};
    struct GDML_DLL IntType {};
    struct GDML_DLL FloatType {};
    struct GDML_DLL StrType {};

    struct GDML_DLL PropType {
        Type type;
        Option<Box<Value>> defaultValue;
        // prop <=> full.path.to.another
        Option<Vec<std::string>> binding;
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
            VoidType,
            BoolType, IntType, FloatType, StrType,
            StructType, NodeType, RefType
        > kind;

        using Value = decltype(kind);

        Type() = default;
        Type(Value const& value) : kind(value) {}

        bool operator==(Type const& other) const;
        Option<Type> getMemberType(std::string const& name) const;
        std::string toString() const;
    };

    struct GDML_DLL PropValue {
        Value value;
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
