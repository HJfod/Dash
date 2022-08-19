#pragma once

#include <string>
#include <utils/Types.hpp>
#include <utils/IO.hpp>
#include <mutex>

namespace gdml {
    enum class LanguageRule {
        DefaultStaticFunctions,
        KeepUsingStatements,
    };

    struct Flags {
        enum : int {
            None           = 0b0,
            LogErrors      = 0b1,
            LogWarnings    = 0b10,
            LogMessages    = 0b100,
            LogDebug       = 0b1000,
            LogTokens      = 0b10000,
            LogAST         = 0b100000,
            PrettifyOutput = 0b1000000,
            LogAsJson      = 0b10000000,
            DryRun         = 0b100000000,
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

    static constexpr const auto DEFAULT_FLAGS =
        Flags::LogErrors | Flags::LogWarnings | Flags::LogMessages | 
        Flags::PrettifyOutput;

    static constexpr const auto DEBUG_FLAGS =
        Flags::LogErrors | Flags::LogWarnings | Flags::LogTokens |
        Flags::LogAST    | Flags::LogMessages | Flags::LogDebug  |
        Flags::PrettifyOutput;

    class GDML {
    protected:
        IO& m_io;
        mutable std::mutex m_ioMutex;
        Flags m_flags;
        std::unordered_map<LanguageRule, bool> m_rules;
        bool m_encounteredErrors = false;
        mutable std::mutex m_errorMutex;

        Error compileFile(SourceFile* input, SourceFile* output);

        void formatLines(LineError const& error);
        
    public:
        GDML(Flags flags = DEFAULT_FLAGS, IO& io = DEFAULT_IO);

        bool getRule(LanguageRule rule) const;
        bool setRule(LanguageRule rule, bool value);
        bool getFlag(Flags::Value flag) const;

        Error compileFile(std::string const& input, std::string const& output);

        void logMessage(LineError const& error);
        void logWarning(LineError const& error);
        void logError(LineError const& error);
        void logDebug(std::string const& message);

        std::mutex& rawIoLock();
        IO& rawIO();

        bool encounteredErrors() const;
    };
}
