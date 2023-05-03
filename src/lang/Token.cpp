#include <lang/Token.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

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
static std::unordered_set<std::string> SPECIAL_IDENTS { "this", "super", "root", "global" };

bool lang::isIdentCh(char ch) {
    return
        // no reserved chars
        INVALID_IDENT_CHARS.find_first_of(ch) == std::string::npos && 
        // no operators
        VALID_OP_CHARS.find_first_of(ch) == std::string::npos &&
        // no spaces
        !std::isspace(ch) &&
        // no nulls
        ch != '\0';
}

bool lang::isIdent(std::string const& ident) {
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

bool lang::isSpecialIdent(std::string const& ident) {
    return SPECIAL_IDENTS.contains(ident);
}

bool lang::isOpCh(char ch) {
    return VALID_OP_CHARS.find_first_of(ch) != std::string::npos;
}

bool lang::isOp(std::string const& op) {
    for (auto& c : op) {
        if (!isOpCh(c)) {
            return false;
        }
    }
    return op.size();
}

bool lang::isUnOp(Op op) {
    switch (op) {
        case Op::Add:
        case Op::Sub:
        case Op::Not:
            return true;
        default: return false;
    }
}

size_t lang::opPriority(Op op) {
    return std::get<1>(OPS.at(op));
}

OpDir lang::opDir(Op op) {
    return std::get<2>(OPS.at(op));
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
        // if it's not a comment nor space, then we're done
        else {
            break;
        }
    }
}

ParseResult<> Token::pullSemicolons(Stream& stream) {
    Rollback rb (stream);
    if (stream.last() == Token(Punct('}'))) {
        while (Token::pull(';', stream)) {}
        rb.commit();
        return Ok();
    }
    else {
        GEODE_UNWRAP(Token::pull(';', stream));
        while (Token::pull(';', stream)) {}
        rb.commit();
        return Ok();
    }
}

ParseResult<Token> Token::pull(Stream& stream) {
    Token::skipToNext(stream);

    Rollback rb(stream);

    auto done = [&](Token const& tk) {
        rb.commit();
        stream.setLastToken(tk);
        return Ok(tk);
    };

    stream.debugTick();
    if (stream.eof()) {
        return rb.errorLastToken("Expected token, found end-of-file");
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
                    return rb.error("Expected escaped character, found end-of-file");
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
                            stream.src()->getLocation(stream.offset() - 1),
                            stream.src()->getLocation(stream.offset())
                        ),
                    }); break;
                }
            }
            // todo: interpolated string literals
            else if (c == '"') {
                break;
            }
        }
        return done(Token(lit));
    }

    // number literals
    // todo: hex
    if (isdigit(stream.peek())) {
        bool foundDot = false;
        std::string num;
        while (auto c = stream.peek()) {
            stream.debugTick();
            if (!(isdigit(c) || (!foundDot && c == '.'))) {
                break;
            }
            if (c == '.') {
                foundDot = true;
            }
            stream.next();
            num += c;
        }
        if (foundDot) {
            try {
                rb.commit();
                return done(Token(std::stod(num)));
            }
            catch(...) {
                return rb.error("Invalid float literal");
            }
        }
        else {
            try {
                rb.commit();
                return done(Token(std::stoul(num)));
            }
            catch(...) {
                return rb.error("Invalid integer literal");
            }
        }
    }

    // punctuation
    if (std::string("()[]{}:;,.@").find_first_of(stream.peek()) != std::string::npos) {
        rb.commit();
        return done(Token(Punct(stream.next())));
    }

    // other
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
                rb.commit();
                return done(Token(op));
            }
        }

        return rb.error("Invalid operator '{}'", ident);
    }

    // special literals
    if (ident == "true") {
        rb.commit();
        return done(Token(Lit(true)));
    }
    if (ident == "false") {
        rb.commit();
        return done(Token(Lit(false)));
    }
    // if (ident == "null") {
    //     rb.commit();
    //     return done(Token(Lit(NullLit())));
    // }

    // keyword
    for (auto const& [kw, va] : KEYWORDS) {
        if (va == ident) {
            rb.commit();
            return done(Token(kw));
        }
    }

    // identifier
    if (isIdent(ident)) {
        rb.commit();
        return done(Token(ident));
    }

    return rb.error("Invalid keyword or identifier '{}'", ident);
}

Option<Token> Token::peek(Stream& stream, size_t offset) {
    Rollback rb(stream);
    while (offset > 0) {
        if (!Token::pull(stream)) {
            rb.clearMessages();
            return None;
        }
        offset -= 1;
    }
    auto tk = Token::pull(stream);
    rb.clearMessages();
    return tk.ok();
}

std::string lang::tokenToString(Keyword kw, bool debug) {
    if (debug) {
        return fmt::format("keyword({})", KEYWORDS.at(kw));
    }
    return KEYWORDS.at(kw);
}

std::string lang::tokenToString(Ident ident, bool debug) {
    if (debug) {
        return fmt::format("identifier({:?})", ident);
    }
    return ident;
}

std::string lang::tokenToString(Lit lit, bool debug) {
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

std::string lang::tokenToString(Op op, bool debug) {
    if (debug) {
        return fmt::format("op({})", std::get<0>(OPS.at(op)));
    }
    return std::get<0>(OPS.at(op));
}

std::string lang::tokenToString(Punct punct, bool debug) {
    if (debug) {
        return fmt::format("punct('{}')", punct);
    }
    return std::string(1, punct);
}

Rollback::Rollback(Stream& stream, std::source_location const loc)
  : m_stream(stream),
    m_offset(stream.offset()),
    m_msgLevel(stream.m_rollbackLevel)
{
    stream.debugTick(loc);
    stream.m_rollbackLevel += 1;
}

Rollback::~Rollback() {
    if (!m_commit) {
        m_stream.navigate(m_offset);
        m_commit = true;
    }
    m_stream.m_rollbackLevel -= 1;
}

void Rollback::clearMessages() {
    // Remove all messages that have been pushed that have a higher or equal msg level
    // This is because rb.error just pushes errors to the stream's message 
    // queue, but sometimes those errors end up being part of a parse tree that's 
    // an optional branch and aren't actually errors but just signal that that 
    // branch isn't valid (like how Expr::pullPrimaryNonCall tries a bunch of 
    // different options)
    ranges::remove(m_stream.m_messages, [=](std::pair<size_t, Message> const& pair) {
        return pair.first >= m_msgLevel;
    });
}

void Rollback::commit() {
    m_commit = true;
    this->clearMessages();
}
