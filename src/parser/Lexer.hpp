#pragma once

#include <string>
#include <vector>
#include <utils/Error.hpp>
#include <utils/Types.hpp>
#include "Token.hpp"

namespace gdml {
    class GDML;

    using LexingResult = LineResult<Tokens>;
    using NumberResult = GenericResult<std::string>;
    using LiteralResult = GenericResult<std::string>;
    using OperatorData = std::tuple<TokenType, std::string>;
    using OperatorOption = Option<OperatorData>;

    class Lexer {
    protected:
        GDML& m_shared;
        SourceFile* m_source;
        size_t m_index = 0;
        Tokens* m_stream = nullptr;

        LiteralResult getName();
        NumberResult getNumber();
        OperatorOption getNumberSuffix();
        OperatorOption getOperator();
        LiteralResult getStringLiteral();

        LineResult<void> getInterpolatedLiteral();
        LineResult<void> getNextToken();
        void skipToNextToken();
        bool hasNextToken() const;

        LineResult<void> getRemainingTokens();

        std::string escapeCharacters(std::string const& data);
        Position positionFromIndex(size_t index);
        static TokenType getNameType(std::string const& name);
        static TokenType getNumberType(std::string const& num);
    
    public:
        Lexer(GDML& shared, SourceFile* source);
        ~Lexer();

        LexingResult tokenize();
        static LexingResult tokenizeWhole(GDML& compiler, SourceFile* source);
        /**
         * Note: This will return true for numbers, 
         * however a string beginning with a number 
         * is not a valid identifier
         */
        static bool isValidIdentifierChar(char c);
    };
}
