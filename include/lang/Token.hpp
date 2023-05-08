#pragma once

#include "Main.hpp"
#include "Src.hpp"

namespace gdml::lang {
    class Stream;

    enum class Keyword {
        For, In, While,
        If, Else, Try,
        Function, Return, Break, Continue, From,
        Struct, Decl, Extends,
        Required, Get, Set, Depends,
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
        Arrow,  // a -> b
        Farrow, // a => b
        Bind,   // a <=> b
    };

    using VoidLit = std::monostate;
    using BoolLit = bool;
    using StrLit = std::string;
    using IntLit = int64_t;
    using FloatLit = double;
    using Lit = std::variant<VoidLit, BoolLit, StrLit, IntLit, FloatLit>;
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

    GDML_DLL std::string tokenToString(Keyword kw, bool debug = false);
    GDML_DLL std::string tokenToString(Ident ident, bool debug = false);
    GDML_DLL std::string tokenToString(Lit lit, bool debug = false);
    GDML_DLL std::string tokenToString(Op op, bool debug = false);
    GDML_DLL std::string tokenToString(Punct punct, bool debug = false);

    enum class OpDir : bool {
        LTR, RTL,
    };

    struct Rollback;

    struct GDML_DLL Token {
        std::variant<Keyword, Op, Lit, Punct, Ident> value;

        Token() = default;
        Token(Token&&) = default;
        Token(Token const&) = default;
        template <class T>
        Token(T const& value) : value(value) {}
        Token(decltype(Token::value) const& value) : value(value) {}
        Token(decltype(Token::value) && value) : value(value) {}
        bool operator==(Token const&) const = default;

        std::string toString(bool debug = false) const;

        static void skipToNext(Stream& stream);
        static ParseResult<> pullSemicolons(Stream& stream);
        static ParseResult<bool> pullSeparator(char separator, char bracket, Stream& stream);
        static ParseResult<Token> pull(Stream& stream);
        static Option<Token> peek(Stream& stream, size_t offset = 0);

        template <class T>
        static ParseResult<T> pull(Stream& stream) {
            Token::skipToNext(stream);
            Rollback rb(stream);
            if (stream.eof()) {
                return rb.errorLastToken("Expected {}, got end-of-file", TokenTraits<T>::TYPE_NAME);
            }
            GEODE_UNWRAP_INTO(auto tk, Token::pull(stream));
            if (auto val = std::get_if<T>(&tk.value)) {
                rb.commit();
                return geode::Ok(*val);
            }
            return rb.error(
                "Expected {}, got '{}'",
                TokenTraits<T>::TYPE_NAME, tk.toString()
            );
        }

        template <class T>
        static ParseResult<T> pull(T c, Stream& stream) {
            Token::skipToNext(stream);
            Rollback rb(stream);
            if (stream.eof()) {
                return rb.errorLastToken("Expected '{}', got end-of-file", tokenToString(c));
            }
            GEODE_UNWRAP_INTO(auto tk, Token::pull(stream));
            if (auto val = std::get_if<T>(&tk.value)) {
                if (*val == c) {
                    rb.commit();
                    return geode::Ok(*val);
                }
            }
            return rb.error(
                "Expected '{}', got '{}'",
                tokenToString(c), tk.toString()
            );
        }

        template <class T>
        static Option<T> draw(Stream& stream) {
            Rollback rb(stream);
            auto res = Token::pull<T>(stream);
            rb.clearMessages();
            if (res.isOk()) {
                rb.commit();
            }
            return res.ok();
        }

        template <class T>
        static Option<T> draw(T c, Stream& stream) {
            Rollback rb(stream);
            auto res = Token::pull<T>(c, stream);
            rb.clearMessages();
            if (res.isOk()) {
                rb.commit();
            }
            return res.ok();
        }

        template <class T>
        static Option<T> peek(Stream& stream, size_t offset = 0) {
            auto value = Token::peek(stream, offset);
            if (value) {
                if (auto pun = std::get_if<T>(&value.value().value)) {
                    return *pun;
                }
            }
            return None;
        }

        template <class T>
        static bool peek(T c, Stream& stream, size_t offset = 0) {
            auto value = Token::peek(stream, offset);
            if (value) {
                if (auto pun = std::get_if<T>(&value.value().value)) {
                    return *pun == c;
                }
            }
            return false;
        }
    };

    GDML_DLL bool isIdentCh(char ch);
    GDML_DLL bool isIdent(std::string const& ident);
    GDML_DLL bool isSpecialIdent(std::string const& ident);
    GDML_DLL bool isOpCh(char ch);
    GDML_DLL bool isOp(std::string const& op);
    GDML_DLL bool isUnOp(Op op);
    GDML_DLL size_t opPriority(Op op);
    GDML_DLL OpDir opDir(Op op);
}
