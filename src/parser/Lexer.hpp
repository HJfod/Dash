#pragma once

#include <string>
#include <vector>
#include <utils/Error.hpp>
#include <utils/Types.hpp>
#include "Token.hpp"

namespace gdml {
    using LexingResult = LineResult<Tokens>;
    using NumberResult = GenericResult<std::string>;
    using LiteralResult = GenericResult<std::string>;
    using OperatorData = std::tuple<TokenType, std::string>;
    using OperatorOption = Option<OperatorData>;
    using EmbeddedCodeData = std::tuple<std::string, std::string>;
    using EmbeddedCodeResult = GenericResult<EmbeddedCodeData>;

    class Lexer {
    protected:
        Instance& m_instance;
        size_t m_index = 0;
        Tokens* m_stream = nullptr;

        LiteralResult getName();
        NumberResult getNumber();
        OperatorOption getNumberSuffix();
        OperatorOption getOperator();
        LiteralResult getStringLiteral();
        EmbeddedCodeResult getEmbeddedCodeBlock();

        LineResult<void> getInterpolatedLiteral();
        LineResult<void> getNextToken();
        void skipToNextToken();
        bool hasNextToken() const;

        LineResult<void> getRemainingTokens();

        std::string escapeCharacters(std::string const& data);
        Position positionFromIndex(size_t index);
        static TokenType getNameType(std::string const& name);
        static TokenType getDataType(std::string const& num);
    
    public:
        Lexer(Instance& instance);
        ~Lexer();

        LexingResult tokenize();

        /**
         * Note: This will return true for numbers, 
         * however a string beginning with a number 
         * is not a valid identifier
         */
        static bool isValidIdentifierChar(char c);
    };
}
