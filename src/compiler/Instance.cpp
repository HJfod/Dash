#include "GDML.hpp"
#include "Instance.hpp"
#include <parser/Parser.hpp>
#include <parser/Lexer.hpp>
#include "Compiler.hpp"
#include <fstream>

using namespace gdml;
using namespace gdml::io;

#define GDML_LINE   "========================"
#define GDML_W_LINE "------------------------"

template<class T>
struct HoldPointer {
    T*& m_hold;
    T m_inst;
    template<class... Args>
    HoldPointer(T*& hold, Args&&... args)
     : m_hold(hold), m_inst(T(std::forward<Args>(args)...)) {
        hold = &m_inst;
    }
    ~HoldPointer() {
        m_hold = nullptr;
    }
};

Instance::Instance(GDML& shared, SourceFile const* src, SourceFile const* dest) noexcept
  : m_shared(shared), m_src(src), m_dest(dest) {}

Error Instance::execute() {
    m_shared.logDebug("Working with source file: " + m_src->path.string());
    m_shared.logDebug("Destination path: " + m_dest->path.string());

    // tokenixing

    m_shared.logDebug("Tokenizing...");

    HoldPointer lexerHolder(m_lexer, *this);
    auto res = m_lexer->tokenize();
    if (!res) {
        auto err = res.unwrapErr();
        m_shared.logError(err);
        return err.code;
    }
    if (m_shared.encounteredErrors()) {
        return Error::ScrollUpAndReadTheConsoleOutput;
    }
    auto tokens = res.unwrap();

    if (m_shared.getFlag(Flags::LogTokens)) {
        std::lock_guard lock(m_shared.rawIoLock());
        m_shared.rawIO() << Color::Cyan << "======== TOKENS ========\n";
        for (auto& token : tokens) {
            auto type = tokenTypeToLongString(token.type);
            auto loc = token.start.toString() + " - " + token.end.toString();
            m_shared.rawIO()
                << type << std::string(19 - type.size(), ' ') << "| "
                << loc << std::string(17 - loc.size(), ' ') << "| "
                << token.data << "\n";
        }
        m_shared.rawIO() << GDML_LINE "\n\n" << Color::White;
    }

    // parsing

    m_shared.logDebug("Parsing...");

    HoldPointer parserHolder(m_parser, *this, tokens);
    auto pres = m_parser->parse();
    if (!pres) {
        auto err = pres.unwrapErr();
        m_shared.logError(err);
        return err.code;
    }

    // unique_ptr so our memory is squeaky clean :3
    auto ast = std::unique_ptr<ast::AST>(pres.unwrap());
    if (m_shared.encounteredErrors()) {
        return Error::ScrollUpAndReadTheConsoleOutput;
    }

    if (m_shared.getFlag(Flags::LogAST)) {
        std::lock_guard lock(m_shared.rawIoLock());
        m_shared.rawIO() << Color::Pink << "========== AST =========\n";
        for (auto& expr : ast->tree()) {
            m_shared.rawIO() << expr->debugPrintAST(0);
        }
        m_shared.rawIO() << GDML_LINE "\n\n" << Color::White;
    }

    // compiling

    m_shared.logDebug("Compiling...");

    HoldPointer compilerHolder(m_compiler, *this, ast.get());
    auto cres = m_compiler->compile();
    if (cres != Error::OK) {
        return cres;
    }
    if (m_shared.encounteredErrors()) {
        return Error::ScrollUpAndReadTheConsoleOutput;
    }

    // codegen

    m_shared.logDebug("Generating code...");

    { // save generated file
        std::ofstream file(m_dest->path, std::ios::out | std::ios::trunc);
        if (!file.is_open()) {
            return Error::CantSaveFile;
        }
        m_compiler->codegen(file);
    }

    return Error::OK;
}

GDML& Instance::getShared() const {
    return m_shared;
}

SourceFile const* Instance::getSource() const {
    return m_src;
}

SourceFile const* Instance::getDestination() const {
    return m_dest;
}

Lexer& Instance::getLexer() const {
    return *m_lexer;
}

Parser& Instance::getParser() const {
    return *m_parser;
}

Compiler& Instance::getCompiler() const {
    return *m_compiler;
}
