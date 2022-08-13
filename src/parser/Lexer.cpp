#include "Lexer.hpp"
#include <sstream>
#include <unordered_map>
#include <iostream>
#include <algorithm>
#include <compiler/GDML.hpp>
#include <array>
#include <compiler/Instance.hpp>

using namespace gdml;

#define THROW_LEX_ERR(code, msg, hint, note) \
    return Err(LineError { Error::code, msg, \
    hint, note, positionFromIndex(startIndex),\
    positionFromIndex(m_index), m_instance.getSource()\
    })

#define THROW_LEX_ERR_FG(err) \
    return Err(LineError { \
    err.code, err.message, err.hint, err.note, \
    positionFromIndex(m_index) })

#define ADD_TOKEN(type, tokdata) \
    m_stream->push_back(std::move(Token { positionFromIndex(startIndex), \
    positionFromIndex(m_index), type, tokdata, m_instance.getSource() }))

bool Lexer::isValidIdentifierChar(char c) {
    // dealing with these ones in a string 
    // is wacky and difficult so they can 
    // just be manually checked first
    if (std::isspace(static_cast<unsigned char>(c))) {
        return false;
    }
    if (c == '\0' || c == '\r') {
        return false;
    }
    static std::string invalid(".,;:-\\/*^~!&$%#\"'<>|+?{}[]()@`=\n\t");
    return std::none_of(invalid.begin(), invalid.end(), [c](auto t) { return c == t; });
}

std::string Lexer::escapeCharacters(std::string const& line) {
    std::string res = line;
    for (auto it = res.begin(); it != res.end(); it++) {
        if (it + 1 >= res.end()) continue;
        if (*it == '\\') {
            it = res.erase(it);
            switch (*it) {
                case 'a': *it = '\a'; break;
                case 'b': *it = '\b'; break;
                case 'f': *it = '\f'; break;
                case 'n': *it = '\n'; break;
                case 'r': *it = '\r'; break;
                case 't': *it = '\t'; break;
                case 'v': *it = '\v'; break;
                case '\'':*it = '\''; break;
                case '"': *it = '"'; break;
                case '\\':*it = '\\'; break;
                default: m_instance.getShared().logWarning({
                    Error::InvalidEscape,
                    "Invalid escape sequence '\\" + std::string(1, *it) + "' at " +
                        std::to_string(std::distance(std::begin(res), it)),
                    "Maybe you meant to escape the '\\'?",
                    "Valid escape sequences: \\a, \\b, \\f, \\n, \\r, \\t, \\v, \\', \\\", \\\\",
                    positionFromIndex(m_index),
                    positionFromIndex(m_index),
                    m_instance.getSource()
                });
            }
        }
    }
    return res;
}

TokenType Lexer::getNameType(std::string const& name) {
    auto tk = knownToken(name);
    if (tk == TokenType::Invalid) {
        return TokenType::Identifier;
    }
    return tk;
}

TokenType Lexer::getNumberType(std::string const& num) {
    return num.find('.') == std::string::npos ? TokenType::Int : TokenType::Float;
}

Position Lexer::positionFromIndex(size_t index) {
    Position pos { 0, 0 };
    for (size_t i = 0; i < index; i++) {
        if (index >= m_instance.getSource()->data.size()) return pos;
        if (m_instance.getSource()->data.at(i) == '\n') {
            pos.line++;
            pos.column = 0;
        } else {
            pos.column++;
        }
    }
    return pos;
}


LiteralResult Lexer::getName() {
    std::string identifier {};
    while (
        m_index < m_instance.getSource()->data.size() &&
        isValidIdentifierChar(m_instance.getSource()->data.at(m_index))
    ) {
        identifier.push_back(m_instance.getSource()->data.at(m_index++));
    }
    return Ok(identifier);
}

NumberResult Lexer::getNumber() {
    // default to decimal verifier
    bool (*verifier)(char) = [](char c) {
        return isdigit(c) || c == '.';
    };

    // consume first character of number
    std::string num { m_instance.getSource()->data.at(m_index++) };
    // check literal base identifier (0x, 0b, etc.)
    if (num.front() == '0') {
        switch (m_instance.getSource()->data.at(m_index)) {
            // hexadecimal
            case 'x': {
                // consume identifier
                num += 'x';
                m_index++;
                // hexadecimal verifier
                verifier = [](char c) {
                    return isdigit(c) ||
                        ('a' <= c && c <= 'f') ||
                        ('A' <= c && c <= 'F');
                };
            } break;

            // binary
            case 'b': {
                // consoom social media
                num += 'b';
                m_index++;
                // binary verifier
                verifier = [](char c) {
                    return c == '0' || c == '1';
                };
            } break;

            default: break;
        }
    }

    bool hasDot = false;
    while (
        m_index < m_instance.getSource()->data.size() &&
        verifier(m_instance.getSource()->data.at(m_index))
    ) {
        auto next = m_index + 1 < m_instance.getSource()->data.size() ? m_instance.getSource()->data.at(m_index + 1) : 0;
        if (m_instance.getSource()->data.at(m_index) == '.') {
            if (hasDot || next == '.') {
                break;
            }
            hasDot = true;
        }
        num.push_back(m_instance.getSource()->data.at(m_index++));
    }
    return Ok(num);
}

OperatorOption Lexer::getNumberSuffix() {
    for (auto& option : types::NUMTYPE_STRS) {
        auto test = m_instance.getSource()->data.substr(m_index, strlen(option));
        if (test == option) {
            // consume suffix
            m_index += test.size();
            return Some(OperatorData { TokenType::NumeralSuffix, test });
        }
    }
    return None;
}

OperatorOption Lexer::getOperator() {
    static constexpr size_t MAX_OPERATOR_SIZE = 3;
    for (size_t i = MAX_OPERATOR_SIZE; i; i--) {
        auto op = m_index + i < m_instance.getSource()->data.size() ?
            m_instance.getSource()->data.substr(m_index, i) :
            m_instance.getSource()->data.substr(m_index);
        auto tk = knownToken(op);
        if (tk != TokenType::Invalid) {
            m_index += i;
            return Some(OperatorData { tk, op });
        }
    }
    return None;
}

LiteralResult Lexer::getStringLiteral() {
    std::string res {};

    auto quoteType = m_instance.getSource()->data.at(m_index);
    // skip quote
    m_index++;

    while (m_index < m_instance.getSource()->data.size()) {
        auto c = m_instance.getSource()->data.at(m_index);
        auto last = m_index ? m_instance.getSource()->data.at(m_index - 1) : 0;
        if (c == quoteType && last != '\\') {
            break;
        }
        if (quoteType == '\'' && c == '\n') {
            return Err(GenericError {
                Error::NoLiteralTerminator,
                "Expected closing '\\'' quote, found newline",
                "Use '\"' for multiline string literals",
                ""
            });
        }
        res += c;
        m_index++;
    }

    // skip ending quote
    m_index++;
    return Ok(escapeCharacters(res));
}

LineResult<void> Lexer::getInterpolatedLiteral() {
    std::string res {};

    // skip quote
    m_index++;
    auto startIndex = m_index;

    ADD_TOKEN(TokenType::BeginInterpolatedString, "");

    size_t braces = 0;
    size_t inComponent = 0;
    while (m_index < m_instance.getSource()->data.size()) {
        auto c = m_instance.getSource()->data.at(m_index);
        auto last = m_index ? m_instance.getSource()->data.at(m_index - 1) : 0;
        if (!inComponent && c == '"' && last != '\\') {
            break;
        }
        // this should work perfectly fine 
        // unless i add a token pair like 
        // '{<' and '>}'
        if (c == '{') {
            // check if escaped
            if (last == '\\') {
                // remove the escape
                res.pop_back();
            } else {
                braces++;
                if (braces == 1) {
                    ADD_TOKEN(TokenType::String, escapeCharacters(res));
                    ADD_TOKEN(TokenType::BeginInterpolatedComponent, "");
                    res.clear();
                    inComponent = m_index;
                    m_index++;
                    continue;
                }
            }
        } else if (c == '}') {
            if (braces) {
                braces--;
                if (!braces) {
                    ADD_TOKEN(TokenType::EndInterpolatedComponent, "");
                    inComponent = 0;
                    m_index++;
                    continue;
                }
            }
        }
        if (inComponent) {
            auto tok = getNextToken();
            if (!tok) return Err(tok.unwrapErr());
            // if this is not done it'll break 
            // when someone puts a space before 
            // the }
            skipToNextToken();
        } else {
            res += c;
            m_index++;
        }
    }

    // skip ending quote
    m_index++;

    if (inComponent) {
        THROW_LEX_ERR(
            NoComponentTerminator,
            "No end for component in interpolated "
            "literal found",
            "",
            "Component opened at " + positionFromIndex(inComponent).toString()
        );
    }
    
    ADD_TOKEN(TokenType::String, escapeCharacters(res));
    ADD_TOKEN(TokenType::EndInterpolatedString, "");

    return Ok();
}

LineResult<void> Lexer::getNextToken() {
    skipToNextToken();

    if (!hasNextToken()) {
        return Ok();
    }

    auto c = m_instance.getSource()->data.at(m_index);
    auto next = m_index + 1 < m_instance.getSource()->data.size() ? m_instance.getSource()->data.at(m_index + 1) : 0;
    auto last = m_index ? m_instance.getSource()->data.at(m_index - 1) : 0;

    auto startIndex = m_index;

    // names (can't start with a number)
    if (isValidIdentifierChar(c) && !isdigit(static_cast<unsigned char>(c))) {
        auto str = getName();
        ADD_TOKEN(getNameType(str.unwrap()), str.unwrap());
        return Ok();
    }
    
    // numbers
    if (isdigit(c)) {
        auto str = getNumber();
        if (!str) {
            auto err = str.unwrapErr();
            THROW_LEX_ERR_FG(err);
        }
        ADD_TOKEN(getNumberType(str.unwrap()), str.unwrap());
        auto suffix = getNumberSuffix();
        if (suffix.has_value()) {
            ADD_TOKEN(
                std::get<0>(suffix.value()),
                std::get<1>(suffix.value())
            );
        }
        return Ok();
    }

    // interpolation
    if (c == '$') {
        if (next != '"') {
            THROW_LEX_ERR(
                UnexpectedToken,
                "Expected string literal after '$'",
                (next == '\'' ? "Change the quote type from (') to (\")" : ""),
                ""
            );
        }
        m_index++;
        return getInterpolatedLiteral();
    }
    
    // string literals
    if (c == '"' || c == '\'') {
        auto str = getStringLiteral();
        if (!str) {
            auto err = str.unwrapErr();
            THROW_LEX_ERR_FG(err);
        }
        ADD_TOKEN(TokenType::String, str.unwrap());
        return Ok();
    }

    // operators
    auto op = getOperator();
    if (op.has_value()) {
        ADD_TOKEN(std::get<0>(op.value()), std::get<1>(op.value()));
        return Ok();
    }

    THROW_LEX_ERR(
        UnknownToken,
        "Unknown token \"" + std::string(1, c) + "\"",
        "Maybe you miswrote an operator?",
        ""
    );
}

void Lexer::skipToNextToken() {
    // skip non-tokens
    while (hasNextToken()) {
        auto c = m_instance.getSource()->data.at(m_index);
        auto next = m_index + 1 < m_instance.getSource()->data.size() ? m_instance.getSource()->data.at(m_index + 1) : 0;
        auto last = m_index ? m_instance.getSource()->data.at(m_index - 1) : 0;

        // spaces
        if (std::isspace(static_cast<unsigned char>(c))) {
            m_index++;
            continue;
        }

        // comment block
        if (c == '/' && next == '*') {
            m_index += 2;
            do { m_index++; } while (
                m_index < m_instance.getSource()->data.size() && !(
                    m_instance.getSource()->data.at(m_index - 1) == '*' &&
                    m_instance.getSource()->data.at(m_index) == '/'
                )
            );
            continue;
        }

        // line comment
        if (c == '/' && next == '/') {
            m_index += 2;
            // wow i can't believe i found an use case for 
            // do-while
            do { m_index++; } while (
                m_index < m_instance.getSource()->data.size() && 
                m_instance.getSource()->data.at(m_index) != '\n'
            );
            continue;
        }

        // newlines
        if (c == '\n') {
            m_index++;
            continue;
        }
    
        // ok well the next one is a 
        // valid token then
        break;
    }
}

bool Lexer::hasNextToken() const {
    return m_index < m_instance.getSource()->data.size();
}

LineResult<void> Lexer::getRemainingTokens() {
    while (hasNextToken()) {
        auto tok = getNextToken();
        if (!tok) return Err(tok.unwrapErr());
    }
    return Ok();
}

Lexer::Lexer(Instance& shared)
  : m_instance(shared),
    m_stream(new Tokens()) {}

Lexer::~Lexer() {
    if (m_stream) {
        delete m_stream;
    }
}

LexingResult Lexer::tokenize() {
    if (!m_instance.getSource()->data.size()) {
        return Ok(Tokens());
    }
    auto p = getRemainingTokens();
    if (!p) return p.unwrapErr();
    return *m_stream;
}

