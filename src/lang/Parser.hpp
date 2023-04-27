#pragma once

#include "Main.hpp"

template <class T>
using ParseResult = Result<T, Message>;

enum class Keyword {
    For, In, While,
    If, Else, Try,
    Function, Return, Break, Continue, From,
    New, Const, Let,
    Export, Import, Extern,
    True, False, Null,
};

enum class Op {
    Seq,    // a = b
    AddSeq, // a += b
    SubSeq, // a -= b
    MulSeq, // a *= b
    DivSeq, // a /= b
    ModSeq, // a %= b
    Add,    // a + b or +a
    Sub,    // a - b or -a
    Mul,    // a * b
    Div,    // a / b
    Mod,    // a % b
    Eq,     // a == b
    Neq,    // a != b
    Less,   // a < b
    Leq,    // a <= b
    More,   // a > b
    Meq,    // a >= b
    Not,    // !a
    And,    // a && b
    Or,     // a || b
    Arrow,  // a => b
};

using NullLit = std::monostate; // std::nullptr_t is not <=>!!!
using BoolLit = bool;
using StrLit = std::string;
using IntLit = int64_t;
using FloatLit = double;
using Lit = std::variant<NullLit, BoolLit, StrLit, IntLit, FloatLit>;
using Ident = std::string;
using Punct = char;

template <class T>
struct TokenTraits;

template <>
struct TokenTraits<Keyword> {
    static inline const char* TYPE_NAME = "keyword";
};

template <>
struct TokenTraits<Op> {
    static inline const char* TYPE_NAME = "operator";
};

template <>
struct TokenTraits<Lit> {
    static inline const char* TYPE_NAME = "literal";
};

template <>
struct TokenTraits<Ident> {
    static inline const char* TYPE_NAME = "identifier";
};

template <>
struct TokenTraits<Punct> {
    static inline const char* TYPE_NAME = "punctuation";
};

template <class T>
concept TokenType = requires {
    TokenTraits<T>::TYPE_NAME;
};

std::string tokenToString(Keyword kw, bool debug = false);
std::string tokenToString(Ident ident, bool debug = false);
std::string tokenToString(Lit lit, bool debug = false);
std::string tokenToString(Op op, bool debug = false);
std::string tokenToString(Punct punct, bool debug = false);

enum class OpDir : bool {
    LTR, RTL,
};

struct Token {
    std::variant<Keyword, Op, Lit, Punct, Ident> value;

    Token() = default;
    Token(Token&&) = default;
    Token(Token const&) = default;
    template <class T>
    Token(T const& value) : value(value) {}
    Token(decltype(Token::value) const& value) : value(value) {}
    Token(decltype(Token::value) && value) : value(value) {}

    std::string toString(bool debug = false) const;

    static void skipToNext(Stream& stream);
    static ParseResult<Token> pull(Stream& stream);
    static Option<Token> peek(Stream& stream);

    template <class T>
    static ParseResult<T> pull(Stream& stream) {
        GEODE_UNWRAP_INTO(auto tk, Token::pull(stream));
        if (auto val = std::get_if<T>(&tk.value)) {
            return Ok(*val);
        }
        return Err(stream.error(fmt::format(
            "Expected {}, got '{}'",
            TokenTraits<T>::TYPE_NAME, tk.toString()
        )));
    }

    template <class T>
    static bool peek(T c, Stream& stream) {
        auto value = Token::peek(stream);
        if (value) {
            if (auto pun = std::get_if<T>(&value.value().value)) {
                return *pun == c;
            }
        }
        return false;
    }
};

bool isIdentCh(char ch);
bool isIdent(std::string const& ident);
bool isSpecialIdent(std::string const& ident);
bool isOpCh(char ch);
bool isOp(std::string const& op);
bool isUnOp(Op op);
