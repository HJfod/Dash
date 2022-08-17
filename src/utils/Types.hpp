#pragma once

#include <vector>
#include <string>
#include "Error.hpp"
#include <external/Result.hpp>
#include <optional>
#include <array>
#include <filesystem>

// Check windows
#if _WIN32 || _WIN64
    #if _WIN64
        #define GDML_IS_64_BIT
    #else
        #define GDML_IS_32_BIT
    #endif
#endif

// Check GCC
#if __GNUC__
    #if __x86_64__ || __ppc64__
        #define GDML_IS_64_BIT
    #else
        #define GDML_IS_32_BIT
    #endif
#endif

namespace gdml {
    class Lexer;
    class Parser;
    class Compiler;
    class Instance;
    class GDML;
    class Type;
    class Value;
    struct Entity;
    struct Variable;
    struct FunctionEntity;

    namespace ast {
        struct Stmt;
        struct VariableDeclExpr;
        struct FunctionDeclStmt;
        class AST;
    }

    constexpr unsigned int hash(const char* str, int h = 0) {
        return !str[h] ? 5381 : (hash(str, h+1) * 33) ^ str[h];
    }

    namespace types {
        enum class DataType {
            Void,
            I8, I16, I32, I64,
            U8, U16, U32, U64,
            F32, F64,
            Bool,
            Char, String,
            Pointer,
            Function,
            Array,
            Class,
        };

        constexpr size_t DATATYPE_COUNT = 18;
        
        constexpr std::array<DataType, DATATYPE_COUNT> DATATYPES {
            DataType::Void,
            DataType::I8, DataType::I16,
            DataType::I32, DataType::I64,
            DataType::U8, DataType::U16,
            DataType::U32, DataType::U64,
            DataType::F32, DataType::F64,
            DataType::Bool,
            DataType::Char, DataType::String,
            DataType::Pointer,
            DataType::Function,
            DataType::Array,
            DataType::Class,
        };

        constexpr std::array<const char*, DATATYPE_COUNT> DATATYPE_STRS {
            "void",
            "i8", "i16", "i32", "i64",
            "u8", "u16", "u32", "u64",
            "f32", "f64",
            "bool",
            "char", "string",
            "@pointer",
            "@function",
            "@array",
            "@class"
        };

        constexpr std::array<const char*, DATATYPE_COUNT> DATATYPE_CPP {
            "void",
            "int8_t", "int16_t", "int32_t", "int64_t",
            "uint8_t", "uint16_t", "uint32_t", "uint64_t",
            "float", "double",
            "bool",
            "char", "gd::string",
            "", "", "", ""
        };

        using I8  = int8_t;
        using I16 = int16_t;
        using I32 = int32_t;
        using I64 = int64_t;

        using U8  = std::uint8_t;
        using U16 = std::uint16_t;
        using U32 = std::uint32_t;
        using U64 = std::uint64_t;

        using F32 = float;
        using F64 = double;

        using Void = void;
        using Bool = bool;
        using Char = char;
        using String = std::string;

        template<class T>
        constexpr DataType getDataType() {
            #define GDML_REAL_TYPE_TO_DATA_TYPE(t) \
                if constexpr (std::is_same_v<T, t>) { return DataType::t; } else
            
            GDML_REAL_TYPE_TO_DATA_TYPE(Void)
            GDML_REAL_TYPE_TO_DATA_TYPE(Bool)
            GDML_REAL_TYPE_TO_DATA_TYPE(Char)
            GDML_REAL_TYPE_TO_DATA_TYPE(String)
            GDML_REAL_TYPE_TO_DATA_TYPE(I8)
            GDML_REAL_TYPE_TO_DATA_TYPE(I16)
            GDML_REAL_TYPE_TO_DATA_TYPE(I32)
            GDML_REAL_TYPE_TO_DATA_TYPE(I64)
            GDML_REAL_TYPE_TO_DATA_TYPE(U8)
            GDML_REAL_TYPE_TO_DATA_TYPE(U16)
            GDML_REAL_TYPE_TO_DATA_TYPE(U32)
            GDML_REAL_TYPE_TO_DATA_TYPE(U64)
            GDML_REAL_TYPE_TO_DATA_TYPE(F32)
            GDML_REAL_TYPE_TO_DATA_TYPE(F64)
            {
                static_assert(!std::is_same_v<T, T>, "Invalid type to convert to enum");
            }
        }

        template<class T>
        using DynamicArray = std::vector<T>;
        template<class T, size_t S>
        using StaticArray = std::array<T, S>;

        std::string dataTypeToString(DataType type);
        std::string dataTypeToCppType(DataType type);
        DataType dataTypeFromString(std::string const& str);
        bool dataTypeIsUnsigned(DataType type);
        
        struct TypeQualifiers {
            bool isConst = false;
            
            constexpr TypeQualifiers() = default;
            constexpr TypeQualifiers(
                bool isConst
            ) : isConst(isConst) {}

            constexpr TypeQualifiers operator|(TypeQualifiers const& other) const {
                return TypeQualifiers(
                    other.isConst || isConst
                );
            }
        };

        // this is much easier to read than slamming
        // `TypeQualifiers { true }` everywhere
        constexpr TypeQualifiers NON_CONST_QUALIFIED = { false };
        constexpr TypeQualifiers CONST_QUALIFIED = { true };

        enum class PointerType {
            Pointer,
            Reference,
            Move,
        };
    }

    template<class T>
    using Option = std::optional<T>;
    static constexpr auto None = std::nullopt;
    template<class T>
    inline constexpr std::optional<T> Some(T&& value) {
        return std::make_optional<T>(value);
    }

    struct GenericError {
        Error code;
        std::string message;
        std::string hint;
        std::string note;
    };
    template<class T>
    using GenericResult = Result<T, GenericError>;

    struct Position {
        size_t line;
        size_t column;

        Position() = default;
        inline Position(size_t line, size_t column)
         : line(line), column(column) {}

        inline std::string toString() const {
            return std::to_string(line + 1) + ":" + std::to_string(column + 1);
        }
    };

    struct SourceFile {
        std::filesystem::path path;
        std::string data;

        std::vector<std::string> linesFrom(Position const& start, Position const& end) const;
    };

    struct LineError {
        Error code;
        std::string message;
        std::string hint;
        std::string note;
        Position start;
        Position end;
        SourceFile const* source;
    };
    template<class T>
    using LineResult = Result<T, LineError>;

    std::ostream& operator<<(std::ostream& stream, Error error);
    std::ostream& operator<<(std::ostream& stream, Position const& pos);

}
