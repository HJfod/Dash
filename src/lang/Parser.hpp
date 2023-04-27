#pragma once

#include "Main.hpp"

template <class T>
using ParseResult = Result<T, Message>;

enum class Kw {
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

enum class OpDir : bool {
    LTR, RTL,
};

std::string tokenToString(Keyword kw, bool debug = false);
std::string tokenToString(Ident ident, bool debug = false);
std::string tokenToString(Lit lit, bool debug = false);
std::string tokenToString(Op op, bool debug = false);
std::string tokenToString(Punct punct, bool debug = false);

