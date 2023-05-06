#include <lang/State.hpp>
#include <lang/Expr.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ParsedSrc::ParsedSrc(Rc<Src> src, Rc<AST> ast) : m_src(src), m_ast(ast) {}

Rc<AST> ParsedSrc::getAST() const {
    return m_ast;
}

UnitParser::UnitParser(Parser& parser) : m_parser(parser), m_scopes({ Scope() }) {}

ParseResult<ParsedSrc> UnitParser::parse(Parser& shared, Rc<Src> src) {
    auto unit = UnitParser(shared);
    auto stream = src->read(unit);
    GEODE_UNWRAP_INTO(auto ast, AST::pull(stream));
    log::debug("Succesfully parsed AST for {}", src->getName());
    log::debug("{}", ast->debug());
    return Ok(ParsedSrc(src, ast));
}

ParseResult<> UnitParser::typecheck(Parser& shared, ParsedSrc const& src) {
    auto unit = UnitParser(shared);
    GEODE_UNWRAP(src.getAST()->typecheck(unit));
    return Ok();
}

Parser& UnitParser::getShared() const {
    return m_parser;
}

void UnitParser::pushType(Type const& type) {
    m_scopes.back().types.insert({ type.toString(), type });
}

Type* UnitParser::getType(std::string const& name, bool topOnly) {
    // Prefer topmost scope
    for (auto& scope : ranges::reverse(m_scopes)) {
        if (scope.types.contains(name)) {
            return &scope.types.at(name);
        }
        if (topOnly) {
            return nullptr;
        }
    }
    return nullptr;
}

void UnitParser::pushVar(Var const& var) {
    m_scopes.back().vars.insert({ var.name, var });
}

Var* UnitParser::getVar(std::string const& name, bool topOnly) {
    // Prefer topmost scope
    for (auto& scope : ranges::reverse(m_scopes)) {
        if (scope.vars.contains(name)) {
            return &scope.vars.at(name);
        }
        if (topOnly) {
            return nullptr;
        }
    }
    return nullptr;
}

void UnitParser::pushScope() {
    m_scopes.emplace_back();
}

void UnitParser::popScope() {
    m_scopes.pop_back();
    if (m_scopes.empty()) {
        throw std::runtime_error("Internal Compiler Error: Scope stack is empty");
    }
}

Parser::Parser(Rc<Src> src) : m_root(src) {
    this->autorelease();
}

Parser* Parser::create(Rc<Src> src) {
    return new Parser(src);
}

Parser* Parser::create(ghc::filesystem::path const& file) {
    auto src = SrcFile::from(file);
    auto ret = Parser::create(src.unwrapOr(nullptr));
    if (!src) {
        ret->log(Message {
            .level = Level::Error,
            .src = nullptr,
            .info = src.unwrapErr(),
            .range = Range(nullptr),
        });
    }
    return ret;
}

void Parser::add(Rc<Src> src) {
    auto state = UnitParser::parse(*this, src);
    if (state) {
        m_srcs.emplace(src, std::move(state.unwrap()));
    }
}

void Parser::compile() {
    if (!m_root) return;
    try {
        if (m_srcs.empty()) {
            this->add(m_root);
        }
        for (auto& [_, parsed] : m_srcs) {
            (void)UnitParser::typecheck(*this, parsed);
        }
    }
    catch(std::exception const& e) {
        this->log(Message {
            .level = Level::Error,
            .src = m_root,
            .info = e.what(),
            .range = Range(m_root->getLocation(0)),
        });
    }
}

void Parser::populate(CCNode* node) {
    this->dispatchLogs();
    if (!this->getErrors().empty()) {
        node->addChild(CCLabelBMFont::create(
            "There were errors loading GDML - see console",
            "bigFont.fnt"
        ));
    }
}

void Parser::dispatchLogs() const {
    for (auto& [_, msg] : m_messages) {
        switch (msg.level) {
            default:
            case Level::Info:    log::info("{}", msg.toString()); break;
            case Level::Error:   log::error("{}", msg.toString()); break;
            case Level::Warning: log::warn("{}", msg.toString()); break;
        }
    }
}

void Parser::log(Message const& message, size_t level) {
    m_messages.push_back({ level, message });
}

size_t Parser::pushLogLevel() {
    return m_rollbackLevel++;
}

void Parser::popLogLevel() {
    m_rollbackLevel -= 1;
}

void Parser::popMessages(size_t level) {
    ranges::remove(m_messages, [=](std::pair<size_t, Message> const& pair) {
        return pair.first >= level;
    });
}

std::vector<Message> Parser::getErrors() const {
    std::vector<Message> errs;
    for (auto& [_, msg] : m_messages) {
        if (msg.level == Level::Error) {
            errs.push_back(msg);
        }
    }
    return errs;
}
