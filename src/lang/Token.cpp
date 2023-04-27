#include "Parser.hpp"

static std::unordered_map<Keyword, std::string> KEYWORDS {
    { Keyword::For,         "for" },
    { Keyword::While,       "while" },
    { Keyword::In,          "in" },
    { Keyword::If,          "if" },
    { Keyword::Else,        "else" },
    { Keyword::Try,         "try" },
    { Keyword::Function,    "function" },
    { Keyword::Return,      "return" },
    { Keyword::Break,       "break" },
    { Keyword::Continue,    "continue" },
    { Keyword::From,        "from" },
    { Keyword::New,         "new" },
    { Keyword::Const,       "const" },
    { Keyword::Let,         "let" },
    { Keyword::Export,      "export" },
    { Keyword::Import,      "import" },
    { Keyword::Extern,      "extern" },
    { Keyword::True,        "true" },
    { Keyword::False,       "false" },
    { Keyword::Null,        "null" },
};

static std::unordered_map<Op, std::tuple<std::string, size_t, OpDir>> OPS {
    { Op::Not,      { "!",  7,  OpDir::RTL } },
    { Op::Mul,      { "*",  6,  OpDir::LTR } },
    { Op::Div,      { "/",  6,  OpDir::LTR } },
    { Op::Mod,      { "%",  6,  OpDir::LTR } },
    { Op::Add,      { "+",  5,  OpDir::LTR } },
    { Op::Sub,      { "-",  5,  OpDir::LTR } },
    { Op::Eq,       { "==", 4,  OpDir::LTR } },
    { Op::Neq,      { "!=", 4,  OpDir::LTR } },
    { Op::Less,     { "<",  4,  OpDir::LTR } },
    { Op::Leq,      { "<=", 4,  OpDir::LTR } },
    { Op::More,     { ">",  4,  OpDir::LTR } },
    { Op::Meq,      { ">=", 4,  OpDir::LTR } },
    { Op::And,      { "&&", 3,  OpDir::LTR } },
    { Op::Or,       { "||", 2,  OpDir::LTR } },
    { Op::ModSeq,   { "%=", 1,  OpDir::RTL } },
    { Op::DivSeq,   { "/=", 1,  OpDir::RTL } },
    { Op::MulSeq,   { "*=", 1,  OpDir::RTL } },
    { Op::SubSeq,   { "-=", 1,  OpDir::RTL } },
    { Op::AddSeq,   { "+=", 1,  OpDir::RTL } },
    { Op::Seq,      { "=",  1,  OpDir::RTL } },
    { Op::Arrow,    { "=>", 0,  OpDir::RTL } },
};

static std::string INVALID_IDENT_CHARS = ".,;(){}[]@`\\´¨'\"";
static std::string VALID_OP_CHARS = "=+-/*<>!#?&|%:~^";
static std::unordered_set<std::string> SPECIAL_IDENTS { "this", "root" };

bool isIdentCh(char ch) {
    return
        // no reserved chars
        INVALID_IDENT_CHARS.find_first_of(ch) == std::string::npos && 
        // no operators
        VALID_OP_CHARS.find_first_of(ch) == std::string::npos &&
        // no spaces
        !std::isspace(ch);
}

bool isIdent(std::string const& ident) {
    // can't be empty
    if (!ident.size()) {
        return false;
    }
    // can't start with a digit
    if (isdigit(ident.front())) {
        return false;
    }
    for (auto& c : ident) {
        if (!isIdentCh(c)) {
            return false;
        }
    }
    // no keywords
    for (auto const& [kw, va] : KEYWORDS) {
        if (ident == va) {
            return false;
        }
    }
    // otherwise fine ig
    return true;
}

bool isSpecialIdent(std::string const& ident) {
    return SPECIAL_IDENTS.contains(ident);
}

bool isOpCh(char ch) {
    return VALID_OP_CHARS.find_first_of(ch) != std::string::npos;
}

bool isOp(std::string const& op) {
    for (auto& c : op) {
        if (!isOpCh(c)) {
            return false;
        }
    }
    return op.size();
}

bool isUnOp(Op op) {
    switch (op) {
        case Op::Add:
        case Op::Sub:
        case Op::Not:
            return true;
        default: return false;
    }
}

std::string Token::toString(bool debug) const {
    return std::visit(makeVisitor {
        [&](auto const& value) {
            return tokenToString(value, debug);
        },
    }, value);
}

void Token::skipToNext(Stream& stream) {
    while (true) {
        stream.debugTick();
        while (std::isspace(stream.peek())) {
            stream.next();
        }
        // comments
        if (stream.peek(2) == "//") {
            while (stream.peek() && stream.next() != '\n') {}
        }
        else if (stream.peek(2) == "/*") {
            while (stream.peek() && (stream.next() != '*' || stream.peek() != '/')) {
                // eat last /
                stream.next();
                // can't do while (next || next) because that causes an infinite 
                // loop at **/
            }
        }
    }
}

ParseResult<Token> Token::pull(Stream& stream) {
    Token::skipToNext(stream);

    stream.debugTick();
    if (stream.eof()) {
        return Err(stream.error("Expected token, found end-of-file"));
    }

    // string literals
    if (stream.peek() == '"') {
        stream.next();
        std::string lit;
        while (auto c = stream.next()) {
            stream.debugTick();
            if (c == '\\') {
                auto escaped = stream.next();
                if (escaped == '\0') {
                    return Err(stream.error("Expected escaped character, found end-of-file"));
                }
                switch (escaped) {
                    case 'n':  lit += '\n'; break;
                    case 'r':  lit += '\r'; break;
                    case 't':  lit += '\t'; break;
                    case '"':  lit += '\"'; break;
                    case '\'': lit += '\''; break;
                    case '\\': lit += '\\'; break;
                    case '{':  lit += '{'; break;
                    default: stream.log(Message {
                        .level = Level::Warning,
                        .src = stream.src(),
                        .info = fmt::format("Unknown escape sequence '\\{}'", escaped),
                        .range = Range(
                            stream.src()->getLocation(stream.offset() - 2),
                            stream.src()->getLocation(stream.offset() - 1)
                        ),
                    });
                }
            }
            // todo: interpolated string literals
            else if (c == '"') {
                break;
            }
        }
        return Ok(Token(lit));
    }

    // number literals
    // todo: hex
    if (isdigit(stream.peek())) {
        bool foundDot = false;
        std::string num;
        auto start = stream.location();
        while (auto c = stream.peek()) {
            if (!(isdigit(c) || (!foundDot && c == '.'))) {
                break;
            }
            if (c == '.') {
                foundDot = true;
            }
            num += c;
        }
        if (foundDot) {
            try {
                return Ok(Token(std::stod(num)));
            }
            catch(...) {
                return Err(stream.error("Invalid float literal", start));
            }
        }
        else {
            try {
                return Ok(Token(std::stoul(num)));
            }
            catch(...) {
                return Err(stream.error("Invalid integer literal", start));
            }
        }
    }

    // punctuation
    if (std::string("()[]{}:;,.@").find_first_of(stream.peek()) != std::string::npos) {
        return Ok(Token(Punct(stream.next())));
    }

    // other
    auto start = stream.location();
    std::string ident;
    while (isIdentCh(stream.peek())) {
        ident += stream.next();
    }

    // idents can't contain any of the same characters as operators
    if (ident.empty()) {
        std::string maybeOp;
        while (isOpCh(stream.peek())) {
            maybeOp += stream.next();
        }
            
        for (auto const& [op, va] : OPS) {
            if (std::get<0>(va) == maybeOp) {
                return Ok(Token(op));
            }
        }

        return Err(stream.error(fmt::format("Invalid operator '{}'", ident), start));
    }

    // special literals
    if (ident == "true") {
        return Ok(Token(Lit(true)));
    }
    if (ident == "false") {
        return Ok(Token(Lit(false)));
    }
    if (ident == "null") {
        return Ok(Token(Lit(NullLit())));
    }

    // keyword
    for (auto const& [kw, va] : KEYWORDS) {
        if (va == ident) {
            return Ok(Token(kw));
        }
    }

    // identifier
    if (isIdent(ident)) {
        return Ok(Token(ident));
    }

    return Err(stream.error(fmt::format("Invalid keyword or identifier '{}'", ident), start));
}

Option<Token> Token::peek(Stream& stream) {
    auto pos = stream.offset();
    auto tk = Token::pull(stream);
    stream.navigate(pos);
    return tk.ok();
}

std::string tokenToString(Keyword kw, bool debug) {
    if (debug) {
        return fmt::format("keyword({})", KEYWORDS.at(kw));
    }
    return KEYWORDS.at(kw);
}

std::string tokenToString(Ident ident, bool debug) {
    if (debug) {
        return fmt::format("identifier({:?})", ident);
    }
    return ident;
}

std::string tokenToString(Lit lit, bool debug) {
    return std::visit(makeVisitor {
        [&](NullLit const& null) -> std::string {
            return "null";
        },
        [&](BoolLit const& b) -> std::string {
            if (debug) {
                return fmt::format("bool({})", b ? "true" : "false");
            }
            return b ? "true" : "false";
        },
        [&](StrLit const& str) -> std::string {
            if (debug) {
                return fmt::format("string({:?})", str);
            }
            return str;
        },
        [&](IntLit const& num) -> std::string {
            if (debug) {
                return fmt::format("int({})", num);
            }
            return geode::utils::numToString(num);
        },
        [&](FloatLit const& num) -> std::string {
            if (debug) {
                return fmt::format("float({})", num);
            }
            return geode::utils::numToString(num);
        },
    }, lit);
}

std::string tokenToString(Op op, bool debug) {
    if (debug) {
        return fmt::format("op({})", std::get<0>(OPS.at(op)));
    }
    return std::get<0>(OPS.at(op));
}

std::string tokenToString(Punct punct, bool debug) {
    if (debug) {
        return fmt::format("punct('{}')", punct);
    }
    return std::string(1, punct);
}
