#include "Token.hpp"
#include <unordered_map>
#include <ostream>

using namespace gdml;

static const std::unordered_map<std::string, TokenType> TOKEN_STRINGS = {
    { "<INT>", TokenType::Int },
    { "<FLOAT>", TokenType::Float },
    { "<STRING>", TokenType::String },

    { "<NUM_SUFFIX>", TokenType::NumeralSuffix },

    { "$", TokenType::Interpolate },
    { "<BEGIN_INTER>", TokenType::BeginInterpolatedString },
    { "<END_INTER>", TokenType::EndInterpolatedString },
    { "<BEGIN_COMPO>", TokenType::BeginInterpolatedComponent },
    { "<END_COMPO>", TokenType::EndInterpolatedComponent },

    { "<EMBED_LANG>", TokenType::EmbedLanguageIdentifier },
    { "<EMBED_CODE>", TokenType::EmbeddedCode },

    { "<IDENTIFIER>", TokenType::Identifier },

    { "template", TokenType::Template },
    { "enum", TokenType::Enum },
    { "auto", TokenType::Let },
    { "let", TokenType::Let },
    { "const", TokenType::Const },
    { "for", TokenType::For },
    { "in", TokenType::In },
    { "while", TokenType::While },
    { "continue", TokenType::Continue },
    { "break", TokenType::Break },
    { "if", TokenType::If },
    { "else", TokenType::Else },
    { "fun", TokenType::Function },
    { "new", TokenType::New },
    { "delete", TokenType::Delete },
    { "struct", TokenType::Struct },
    { "class", TokenType::Class },
    { "static", TokenType::Static },
    { "typeof", TokenType::TypeOf },
    { "this", TokenType::This },
    { "super", TokenType::Super },
    { "macro", TokenType::Macro },
    { "return", TokenType::Return },
    { "impl", TokenType::Implement },
    { "using", TokenType::Using },
    { "namespace", TokenType::Namespace },
    { "unwrap", TokenType::Unwrap },
    { "try", TokenType::Try },
    { "catch", TokenType::Catch },
    { "throw", TokenType::Throw },
    { "true", TokenType::True },
    { "false", TokenType::False },
    { "none", TokenType::None },
    { "null", TokenType::Null },
    { "default", TokenType::Default },
    { "as", TokenType::As },
    { "is", TokenType::Is },
    { "from", TokenType::From },
    { "import", TokenType::Import },
    { "export", TokenType::Export },
    { "extern", TokenType::Extern },
    
    { "=", TokenType::Assign },
    { "+", TokenType::Add },
    { "-", TokenType::Sub },
    { "*", TokenType::Mul },
    { "/", TokenType::Div },
    { "%", TokenType::Mod },
    { "+=", TokenType::AddAssign },
    { "-=", TokenType::SubAssign },
    { "*=", TokenType::MulAssign },
    { "/=", TokenType::DivAssign },
    { "%=", TokenType::ModAssign },
    { "**", TokenType::Pow },
    { "++", TokenType::Increment },
    { "--", TokenType::Decrement },

    { "&&", TokenType::And },
    { "||", TokenType::Or },
    { "!", TokenType::Not },

    { "==", TokenType::Equal },
    { "!=", TokenType::NotEqual },
    { "<", TokenType::LessThan },
    { ">", TokenType::GreaterThan },
    { "<=", TokenType::LessThanOrEqual },
    { ">=", TokenType::GreaterThanOrEqual },

    { "->", TokenType::Arrow },
    { "=>", TokenType::FatArrow },

    { "&", TokenType::BitAnd },
    { "|", TokenType::BitOr },
    { "~", TokenType::BitNot },
    { "^", TokenType::BitXor },
    { "&=", TokenType::BitAndAssign },
    { "|=", TokenType::BitOrAssign },
    { "<<", TokenType::BitShiftLeft },
    { ">>", TokenType::BitShiftRight },

    { "?", TokenType::Question },
    { ":", TokenType::Colon },
    { "??", TokenType::DoubleQuestion },
    { "?=", TokenType::QuestionAssign },

    { "..", TokenType::DotDot },
    { "...", TokenType::DotDotDot },

    { ".", TokenType::Dot },
    { "?->", TokenType::OptionalArrow },
    { ",", TokenType::Comma },
    { "@", TokenType::At },
    { "<SPACE>", TokenType::Space },
    { "<TAB>", TokenType::Tab },
    { "<NEWLINE>", TokenType::Newline },
    { ";", TokenType::SemiColon },
    { "::", TokenType::Scope },

    { "/>", TokenType::InlineCloseTag },
    { "</", TokenType::CloseTag },

    { "[[", TokenType::LeftAttribute },
    { "]]", TokenType::RightAttribute },

    { "(", TokenType::LeftParen },
    { ")", TokenType::RightParen },
    { "{", TokenType::LeftBrace },
    { "}", TokenType::RightBrace },
    { "[", TokenType::LeftBracket },
    { "]", TokenType::RightBracket },

    { "__debug_dump_entities", TokenType::DebugDumpEntities },

    { "<INVALID>", TokenType::Invalid },
};

// same as C++ lol
// https://en.cppreference.com/w/cpp/language/operator_precedence
static const std::unordered_map<TokenType, int> OPERATOR_PRESEDENCE {
    { TokenType::Scope, 150 },

    { TokenType::Increment, 140 },
    { TokenType::Decrement, 140 },
    { TokenType::LeftParen, 140 },
    { TokenType::LeftBracket, 140 },
    { TokenType::Dot, 140 },
    { TokenType::Arrow, 140 },
    { TokenType::OptionalArrow, 140 },

    { TokenType::BitNot, 130 },
    { TokenType::Not, 130 },

    { TokenType::Pow, 120 },

    { TokenType::Mul, 110 },
    { TokenType::Div, 110 },
    { TokenType::Mod, 110 },

    { TokenType::Add, 100 },
    { TokenType::Sub, 100 },

    { TokenType::BitShiftLeft, 95 },
    { TokenType::BitShiftRight, 95 },

    { TokenType::LessThan, 90 },
    { TokenType::LessThanOrEqual, 90 },
    { TokenType::GreaterThan, 90 },
    { TokenType::GreaterThanOrEqual, 90 },

    { TokenType::Equal, 80 },
    { TokenType::NotEqual, 80 },

    { TokenType::BitAnd, 70 },

    { TokenType::BitXor, 60 },

    { TokenType::BitOr, 50 },

    { TokenType::And, 40 },
    { TokenType::Or, 40 },

    { TokenType::Question, 30 },
    { TokenType::DoubleQuestion, 30 },
    { TokenType::Colon, 30 },
    { TokenType::Assign, 30 },
    { TokenType::AddAssign, 30 },
    { TokenType::SubAssign, 30 },
    { TokenType::MulAssign, 30 },
    { TokenType::DivAssign, 30 },
    { TokenType::ModAssign, 30 },
    { TokenType::BitOrAssign, 30 },
    { TokenType::BitAndAssign, 30 },
    { TokenType::BitXorAssign, 30 },
    { TokenType::QuestionAssign, 30 },
};

std::string gdml::tokenTypeToString(TokenType type) {
    for (auto& str : TOKEN_STRINGS) {
        if (type == str.second) {
            return str.first;
        }
    }
    return "<UNNAMED>";
}

TokenType gdml::knownToken(std::string const& name) {
    if (TOKEN_STRINGS.count(name)) {
        return TOKEN_STRINGS.at(name);
    }
    return TokenType::Invalid;
}

TokenType gdml::inverseParen(TokenType type) {
    switch (type) {
        case TokenType::LeftParen: return TokenType::RightParen;
        case TokenType::RightParen: return TokenType::LeftParen;
        case TokenType::LeftBrace: return TokenType::RightBrace;
        case TokenType::RightBrace: return TokenType::LeftBrace;
        case TokenType::LeftBracket: return TokenType::RightBracket;
        case TokenType::RightBracket: return TokenType::LeftBracket;
        default: return TokenType::Invalid;
    }
}

int gdml::precedenceOfOperator(TokenType type) {
    if (OPERATOR_PRESEDENCE.count(type)) {
        return OPERATOR_PRESEDENCE.at(type);
    }
    return NO_PRESEDENCE;
}

std::string gdml::tokenTypeToLongString(TokenType type) {
    return std::to_string(static_cast<int>(type)) + " (" + tokenTypeToString(type) + ")";
}

size_t gdml::extractSymbolCount(TokenType type, char symbol, bool mustBeAll) {
    auto s = tokenTypeToString(type);
    auto res = std::count(s.begin(), s.end(), symbol);
    if (mustBeAll && res != s.size()) {
        return 0;
    }
    return res;
}

bool gdml::isTernaryOperator(TokenType type) {
    return type == TokenType::Question;
}

bool gdml::isBinaryOperator(TokenType type) {
    #define IS_BETWEEN(lhs, rhs) \
        (static_cast<int>(TokenType::lhs) <= static_cast<int>(type) &&\
        static_cast<int>(type) <= static_cast<int>(TokenType::rhs))
    
    return
        IS_BETWEEN(Assign, Pow) ||
        IS_BETWEEN(And, GreaterThanOrEqual) ||
        IS_BETWEEN(BitAnd, BitShiftRight) ||
        type == TokenType::DoubleQuestion ||
        type == TokenType::QuestionAssign;
}

bool gdml::isMemberOperator(TokenType type) {
    return
        type == TokenType::Dot ||
        type == TokenType::Arrow ||
        type == TokenType::OptionalArrow;
}

bool gdml::isUnaryPrefixOperator(TokenType type) {
    return
        type == TokenType::Add ||
        type == TokenType::Sub ||
        type == TokenType::Increment ||
        type == TokenType::Decrement ||
        type == TokenType::Interpolate ||
        type == TokenType::BitNot ||
        type == TokenType::Not;
}

bool gdml::isUnarySuffixOperator(TokenType type) {
    return
        type == TokenType::Increment ||
        type == TokenType::Decrement;
}

bool gdml::isLiteralPrefixOperator(TokenType type) {
    return 
        type == TokenType::Add ||
        type == TokenType::Sub ||
        type == TokenType::Interpolate;
}

bool gdml::isLiteral(TokenType type) {
    return
        type == TokenType::String ||
        type == TokenType::Float ||
        type == TokenType::Int;
}

bool gdml::isLValueOperator(TokenType type) {
    return
        type == TokenType::Assign ||
        type == TokenType::AddAssign ||
        type == TokenType::DivAssign ||
        type == TokenType::ModAssign ||
        type == TokenType::MulAssign ||
        type == TokenType::SubAssign ||
        type == TokenType::BitOrAssign ||
        type == TokenType::BitAndAssign ||
        type == TokenType::BitXorAssign ||
        type == TokenType::QuestionAssign ||
        type == TokenType::Increment ||
        type == TokenType::Decrement;
}

bool gdml::isDebugToken(TokenType type) {
    return
        type == TokenType::DebugDumpEntities;
}

std::ostream& gdml::operator<<(std::ostream& stream, TokenType type) {
    return stream << static_cast<int>(type) << " (" << tokenTypeToString(type) << ")";
}
