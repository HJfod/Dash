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
    class GDML;

    constexpr unsigned int hash(const char* str, int h = 0) {
        return !str[h] ? 5381 : (hash(str, h+1) * 33) ^ str[h];
    }

    namespace types {
        enum class NumberType {
            I8, I16, I32, I64,
            U8, U16, U32, U64,
            F32, F64,
        };
        
        static std::array<NumberType, 10> NUMTYPES {
            NumberType::I8, NumberType::I16,
            NumberType::I32, NumberType::I64,
            NumberType::U8, NumberType::U16,
            NumberType::U32, NumberType::U64,
            NumberType::F32, NumberType::F64,
        };

        static std::array<const char*, 10> NUMTYPE_STRS {
            "i8", "i16", "i32", "i64",
            "u8", "u16", "u32", "u64",
            "f32", "f64"
        };

        std::string numberTypeToString(NumberType type);
        std::string numberTypeToCppType(NumberType type);
        NumberType numberTypeFromString(std::string const& str);
        bool numberTypeIsUnsigned(NumberType type);

    #ifdef GDML_IS_32_BIT

        using I8  = char;
        using I16 = short;
        using I32 = int;
        using I64 = long long;

        using U8  = unsigned char;
        using U16 = unsigned short;
        using U32 = unsigned int;
        using U64 = unsigned long long;

        using F32 = float;
        using F64 = double;

    #else
        #error "No types defined for 64-bit"
    #endif
        
        using Bool = bool;

        using String = std::string;

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
