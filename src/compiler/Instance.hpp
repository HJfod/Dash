#pragma once

#include <utils/Types.hpp>
#include <parser/Token.hpp>

namespace gdml {
    class Instance {
    protected:
        GDML& m_shared;
        SourceFile const* m_src;
        SourceFile const* m_dest;
        Lexer* m_lexer;
        Parser* m_parser;
        Compiler* m_compiler;
    
    public:
        Instance(GDML& shared, SourceFile const* src, SourceFile const* dest) noexcept;

        Error execute();

        GDML& getShared() const;
        SourceFile const* getSource() const;
        SourceFile const* getDestination() const;
        Lexer& getLexer() const;
        Parser& getParser() const;
        Compiler& getCompiler() const;
    };
}
