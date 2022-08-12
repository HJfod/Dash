#pragma once

#include <string>
#include <utils/Types.hpp>
#include <utils/IO.hpp>

namespace gdml {
    enum class LanguageRule {
        DefaultStaticFunctions,
    };

    struct Flags {
        enum : int {
            None           = 0b0,
            LogErrors      = 0b1,
            LogWarnings    = 0b10,
            LogMessages    = 0b100,
            LogDebug       = 0b100,
            LogTokens      = 0b1000,
            LogAST         = 0b10000,
            PrettifyOutput = 0b100000,
        };
        using Value = decltype(None);

        int m_value = None;

        inline Flags& operator|(Value value) {
            m_value |= value;
            return *this;
        }
        inline bool operator&(Value value) const {
            return m_value & value;
        }
        inline bool operator^(Value value) const {
            return !(m_value & value);
        }

        inline Flags(int flags) : m_value(flags) {}
        inline Flags(Value flags) : m_value(flags) {}
    };

    #ifdef GDML_DEBUG
    static constexpr const auto DEFAULT_FLAGS =
        Flags::LogErrors | Flags::LogWarnings | Flags::LogTokens |
        Flags::LogAST    | Flags::LogMessages | Flags::LogDebug  |
        Flags::PrettifyOutput;
    #else
    static constexpr const auto DEFAULT_FLAGS =
        Flags::LogErrors | Flags::LogWarnings | Flags::LogMessages | 
        Flags::PrettifyOutput;
    #endif

    class GDML {
    protected:
        IO& m_io;
        Flags m_flags;
        SourceFile* m_src = nullptr;
        SourceFile* m_dest = nullptr;
        Lexer* m_lexer = nullptr;
        Parser* m_parser = nullptr;
        Compiler* m_compiler = nullptr;
        std::unordered_map<LanguageRule, bool> m_rules;
        bool m_encounteredErrors = false;

        Error compileFile(SourceFile* input, SourceFile* output);

        void formatLines(LineError const& error);
        
    public:
        GDML(Flags flags = DEFAULT_FLAGS, IO& io = DEFAULT_IO);

        bool getRule(LanguageRule rule) const;
        bool setRule(LanguageRule rule, bool value);
        bool getFlag(Flags::Value flag) const;

        Error compileFile(std::string const& input, std::string const& output);

        SourceFile* getInputFile() const;
        SourceFile* getOutputFile() const;
        Lexer* getLexer() const;
        Parser* getParser() const;
        Compiler* getCompiler() const;

        void logMessage(LineError const& error);
        void logWarning(LineError const& error);
        void logError(LineError const& error);

        IO& io();
    };
}
