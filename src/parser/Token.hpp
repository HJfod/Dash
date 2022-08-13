#pragma once

#include <string>
#include <vector>
#include <utils/Types.hpp>

namespace gdml {
    enum class TokenType {
        // literals
        Int,
        Float,
        String,

        // numeral suffixes
        NumeralSuffix,

        // string interpolation
        Interpolate,    // $
        BeginInterpolatedString,
        EndInterpolatedString,
        BeginInterpolatedComponent,
        EndInterpolatedComponent,

        // identifier
        Identifier,

        // keywords
        Template,   // template
        Enum,       // enum
        Let,        // let
        Const,      // const
        For,        // for
        In,         // in
        While,      // while
        Continue,   // continue
        Break,      // break
        If,         // if
        Else,       // else
        Function,   // fun
        New,        // new
        Delete,     // delete
        Struct,     // struct
        Class,      // class
        Static,     // static
        TypeOf,     // typeof
        This,       // this
        Super,      // super
        Macro,      // macro
        Return,     // return
        Implement,  // impl
        Using,      // using
        Namespace,  // namespace
        Unwrap,     // unwrap
        Try,        // try
        Catch,      // catch
        Throw,      // throw
        True,       // true
        False,      // false
        None,       // none
        Null,       // null
        As,         // as
        Is,         // is
        From,       // from
        Import,     // import
        Export,     // export
        Extern,     // extern

        // operators
        Assign,     // =

        // arithmetic
        Add,        // +
        Sub,        // -
        Mul,        // *
        Div,        // /
        Mod,        // %
        AddAssign,  // +=
        SubAssign,  // -=
        MulAssign,  // *=
        DivAssign,  // /=
        ModAssign,  // %=
        Pow,        // **

        // unary
        Increment,   // iden++
        Decrement,   // iden--

        // logic
        And,                // &&
        Or,                 // ||
        Not,                // !
        Equal,              // ==
        NotEqual,           // !=
        LessThan,           // <
        GreaterThan,        // >
        LessThanOrEqual,    // <=
        GreaterThanOrEqual, // >=

        // function
        Arrow,      // ->
        FatArrow,   // =>

        // macros
        Code,   // `

        // bitwise
        BitAnd,     // &
        BitOr,      // |
        BitNot,     // ~
        BitXor,     // ^
        BitAndAssign,// &=
        BitOrAssign, // |=
        BitNotAssign,// ~=
        BitXorAssign,// ^=
        BitShiftLeft,  // <<
        BitShiftRight, // >>

        // conditional
        Question,       // ?
        Colon,          // :
        DoubleQuestion, // ??
        QuestionAssign, // ?=

        // range & variadic
        DotDot,     // ..
        DotDotDot,  // ...

        // separators
        Dot,        // .
        OptionalDot,// ?.
        Comma,      // ,
        At,         // @
        Space,      // \s
        Tab,        // \t
        Newline,    // \n
        SemiColon,  // ;
        Scope,      // ::

        // xml
        InlineCloseTag, // />   
        CloseTag,       // </   

        // attributes
        LeftAttribute,  // [[
        RightAttribute, // ]]

        // parenthesis
        LeftParen,      // (
        RightParen,     // )
        LeftBrace,      // {
        RightBrace,     // }
        LeftBracket,    // [
        RightBracket,   // ]

        Invalid,

        TOKEN_ENUM_END
    };
    static constexpr auto TOKEN_COUNT = static_cast<size_t>(TokenType::TOKEN_ENUM_END);

    std::string tokenTypeToString(TokenType type);
    std::string tokenTypeToLongString(TokenType type);
    TokenType knownToken(std::string const& name);
    TokenType inverseParen(TokenType type);
    int precedenceOfOperator(TokenType type);

    // in case it is needed to parse something like '**' 
    // as two of '*', use this to enumerate the amount of 
    // instances and then hop on to the next token
    size_t extractSymbolCount(TokenType type, char symbol);

    // token categorization
    bool isTernaryOperator(TokenType type);
    bool isBinaryOperator(TokenType type);
    bool isUnaryPrefixOperator(TokenType type);
    bool isUnarySuffixOperator(TokenType type);
    bool isLiteralPrefixOperator(TokenType type);
    bool isLiteral(TokenType type);

    std::ostream& operator<<(std::ostream& stream, TokenType type);

    static constexpr const int NO_PRESEDENCE = -1;

    struct Token {
        Position start;
        Position end;
        TokenType type;
        std::string data;
        Option<std::string> rawData;
        SourceFile const* source;

        Token(
            Position const& start,
            Position const& end,
            TokenType type,
            std::string const& data,
            std::string const& rawData,
            SourceFile const* source
        ) : start(start),
            end(end),
            type(type),
            data(data),
            rawData(rawData),
            source(source) {}
        
        Token(
            Position const& start,
            Position const& end,
            TokenType type,
            std::string const& data,
            SourceFile const* source
        ) : start(start),
            end(end),
            type(type),
            data(data),
            rawData(None),
            source(source) {}
        
        static Token invalid(SourceFile const* src) {
            return Token({ 0, 0 }, { 0, 0 }, TokenType::Invalid, "", src);
        }
    };
    using Tokens = std::vector<Token>;
}
