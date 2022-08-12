#include "GDML.hpp"
#include <utils/Types.hpp>
#include <fstream>
#include "Compiler.hpp"
#include <parser/Parser.hpp>
#include <parser/Lexer.hpp>

using namespace gdml;
using namespace gdml::io;

#define GDML_LINE   "========================"
#define GDML_W_LINE "------------------------"

// i love raii
template<class T>
struct HoldPointer {
    T*& m_pointer;

    HoldPointer(T*& ptr, T* val) : m_pointer(ptr) {
        ptr = val;
    }
    ~HoldPointer() {
        m_pointer = nullptr;
    }
};

// this is a macro and not a function because 
// i couldn't get passing GDML& through variadic 
// template args to work 
#define MAKE_TOOL(var, type, ...) \
    auto var = type(__VA_ARGS__);\
    HoldPointer var##Holder(m_##var, &var);

std::unordered_map<LanguageRule, bool> DEFAULT_RULES = {
    { LanguageRule::DefaultStaticFunctions, true },
};

GDML::GDML(Flags flags, IO& io)
 : m_flags(flags), m_io(io), m_rules(DEFAULT_RULES) {}

Error GDML::compileFile(SourceFile* src, SourceFile* dest) {
    if (m_flags & Flags::LogDebug) {
        m_io << "[DEBUG] Working with source file: " << src->path << "\n";
        m_io << "[DEBUG] Destination path: " << dest->path << "\n";
    }

    HoldPointer srcHolder(m_src, src);
    HoldPointer dstHolder(m_dest, dest);
    
    // tokenixing

    if (m_flags & Flags::LogDebug) m_io << "[DEBUG] Tokenizing...\n";

    MAKE_TOOL(lexer, Lexer, *this, m_src);
    auto res = m_lexer->tokenize();
    if (!res) {
        auto err = res.unwrapErr();
        logError(err);
        return err.code;
    }
    if (m_encounteredErrors) {
        return Error::ScrollUpAndReadTheConsoleOutput;
    }
    auto tokens = res.unwrap();

    if (m_flags & Flags::LogTokens) {
        m_io << Color::Cyan << "======== TOKENS ========\n";
        for (auto& token : tokens) {
            auto type = tokenTypeToLongString(token.type);
            auto loc = token.start.toString() + " - " + token.end.toString();
            m_io
                << type << std::string(19 - type.size(), ' ') << "| "
                << loc << std::string(17 - loc.size(), ' ') << "| "
                << token.data << "\n";
        }
        m_io << GDML_LINE "\n\n" << Color::White;
    }

    // parsing

    if (m_flags & Flags::LogDebug) m_io << "[DEBUG] Parsing...\n";

    MAKE_TOOL(parser, Parser, *this, tokens);
    auto pres = m_parser->parse();
    if (!pres) {
        auto err = pres.unwrapErr();
        logError(err);
        return err.code;
    }

    // unique_ptr so our memory is squeaky clean :3
    auto ast = std::unique_ptr<ast::AST>(pres.unwrap());
    if (m_encounteredErrors) {
        return Error::ScrollUpAndReadTheConsoleOutput;
    }

    if (m_flags & Flags::LogAST) {
        m_io << Color::Pink << "========== AST =========\n";
        for (auto& expr : ast->tree()) {
            m_io << expr->debugPrintAST(0);
        }
        m_io << GDML_LINE "\n\n" << Color::White;
    }

    // compiling

    if (m_flags & Flags::LogDebug) m_io << "[DEBUG] Compiling...\n";

    MAKE_TOOL(compiler, Compiler, *this, ast.get());
    auto cres = m_compiler->compile();
    if (cres != Error::OK) {
        return cres;
    }
    if (m_encounteredErrors) {
        return Error::ScrollUpAndReadTheConsoleOutput;
    }

    // codegen

    if (m_flags & Flags::LogDebug) m_io << "[DEBUG] Generating code...\n";

    { // save generated file
        std::ofstream file(m_dest->path, std::ios::out | std::ios::trunc);
        if (!file.is_open()) {
            return Error::CantSaveFile;
        }
        m_compiler->codegen(file);
    }

    if (m_flags & Flags::PrettifyOutput) {

    }
    
    return Error::OK;
}

Error GDML::compileFile(std::string const& input, std::string const& output) {
    std::ifstream file(input);
    if (file.is_open()) {
        SourceFile src {
            input,
            std::string(
                (std::istreambuf_iterator<char>(file)),
                std::istreambuf_iterator<char>()
            )
        };
        SourceFile dest {
            output,
            std::string()
        };
        return compileFile(&src, &dest);
    }
    return Error::CantOpenFile;
}

bool GDML::getFlag(Flags::Value flag) const {
    return m_flags & flag;
}

SourceFile* GDML::getInputFile() const {
    return m_src;
}

SourceFile* GDML::getOutputFile() const {
    return m_dest;
}

Lexer* GDML::getLexer() const {
    return m_lexer;
}

Parser* GDML::getParser() const {
    return m_parser;
}

Compiler* GDML::getCompiler() const {
    return m_compiler;
}

void GDML::formatLines(LineError const& error) {
    size_t lineNum = error.start.line;
    auto linePadding = std::to_string(error.end.line + 1).size();

    for (auto& line : error.source->linesFrom(error.start, error.end)) {
        auto lineNumStr = std::to_string(lineNum + 1);

        // line number
        m_io
            // padding
            << std::string(linePadding - lineNumStr.size(), ' ')
            << Color::Lemon << lineNumStr
            << Color::Gray << " | "
            << Color::White << line << "\n";
        
        size_t squigglesStart;
        size_t squigglesCount;
        // first line
        if (lineNum == error.start.line) {
            // one line error?
            if (error.start.line == error.end.line) {
                // from start to end
                squigglesCount = error.end.column - error.start.column;
                squigglesStart = error.start.column;
            } else {
                // from start to end of line
                squigglesCount = line.size() - error.start.column;
                squigglesStart = error.start.column;
            }
        }
        // last line
        else if (lineNum == error.end.line) {
            squigglesCount = error.end.column;
            squigglesStart = 0;
        }
        // mid line
        else {
            squigglesCount = line.size();
            squigglesStart = 0;
        }

        // squiggles
        m_io
            << Color::Red
            << std::string(linePadding + 3 + squigglesStart, ' ')
            << std::string(squigglesCount, '~')
            << "\n";

        lineNum++;
    }
    
    // reset color
    m_io.color(Color::White);
}

void GDML::logError(LineError const& error) {
    if (m_flags ^ Flags::LogErrors) return;

    m_encounteredErrors = true;

    m_io
        << Color::Gray << GDML_W_LINE "\n"
        << Color::Red
        << "Error " << error.code
            << " in " << Color::Lemon << error.source->path << Color::Red
            << " from " << error.start << " to " << error.end
            << Color::Red << ":\n";
    
    formatLines(error);

    m_io
        << Color::Red << error.message
        << Color::Cyan  << (error.hint.size() ? "\n(Hint: " + error.hint + ") " : "")
        << Color::Lemon << (error.note.size() ? "\n(Note: " + error.note + ") " : "")
        << "\n\n" << Color::White;
}

void GDML::logWarning(LineError const& error) {
    if (m_flags ^ Flags::LogWarnings) return;

    m_io
        << Color::Gray << GDML_W_LINE "\n"
        << Color::Yellow
        << "Warning " << error.code
            << " in " << Color::Lemon << error.source->path << Color::Red
            << " from " << error.start << " to " << error.end
            << Color::Yellow << ":\n";
    
    formatLines(error);

    m_io
        << Color::Yellow << error.message
        << Color::Cyan  << (error.hint.size() ? "\n(Hint: " + error.hint + ") " : "")
        << Color::Lemon << (error.note.size() ? "\n(Note: " + error.note + ") " : "")
        << "\n\n" << Color::White;
}

void GDML::logMessage(LineError const& error) {
    if (m_flags ^ Flags::LogMessages) return;

    m_io
        << Color::Gray << GDML_W_LINE "\n"
        << Color::Yellow
        << "Message " << error.code
            << " in " << Color::Lemon << error.source->path << Color::Red
            << " from " << error.start << " to " << error.end
            << Color::Yellow << ":\n";
    
    formatLines(error);

    m_io
        << Color::Yellow << error.message
        << Color::Cyan  << (error.hint.size() ? "\n(Hint: " + error.hint + ") " : "")
        << Color::Lemon << (error.note.size() ? "\n(Note: " + error.note + ") " : "")
        << "\n\n" << Color::White;
}

IO& GDML::io() {
    return m_io;
}

bool GDML::getRule(LanguageRule rule) const {
    if (!m_rules.count(rule)) return false;
    return m_rules.at(rule);
}

bool GDML::setRule(LanguageRule rule, bool value) {
    auto old = m_rules.at(rule);
    m_rules.at(rule) = value;
    return old;
}
