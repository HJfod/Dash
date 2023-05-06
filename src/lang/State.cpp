#include <lang/State.hpp>
#include <lang/Expr.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

SrcParser::SrcParser() : m_scopes({ Scope() }) {}

ParseResult<Rc<SrcParser>> SrcParser::parse(Rc<SharedParser> parser, Rc<Src> src) {
    auto state = std::make_shared<SrcParser>();
    state->m_src = src;
    state->m_parser = parser;
    auto stream = src->read(state);
    GEODE_UNWRAP_INTO(state->m_ast, AST::pull(stream));
    log::debug("Succesfully parsed AST for {}", src->getName());
    log::debug("{}", state->m_ast->debug());
    return Ok(state);
}

Rc<SharedParser> SrcParser::getShared() const {
    return m_parser;
}

void SrcParser::pushType(Type const& type) {
    m_scopes.back().types.insert({ type.toString(), type });
}

Type* SrcParser::getType(std::string const& name, bool topOnly) {
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

void SrcParser::pushVar(Var const& var) {
    m_scopes.back().vars.insert({ var.name, var });
}

Var* SrcParser::getVar(std::string const& name, bool topOnly) {
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

void SrcParser::pushScope() {
    m_scopes.emplace_back();
}

void SrcParser::popScope() {
    m_scopes.pop_back();
    if (m_scopes.empty()) {
        throw std::runtime_error("Internal Compiler Error: Scope stack is empty");
    }
}

SharedParser::SharedParser(Rc<Src> src) : m_root(src) {}

Rc<SharedParser> SharedParser::create(Rc<Src> src) {
    return std::make_shared<SharedParser>(src);
}

ParseResult<> SharedParser::add(Rc<Src> src) {
    GEODE_UNWRAP_INTO(m_states[src], SrcParser::parse(shared_from_this(), src));
    return Ok();
}

ParseResult<> SharedParser::compile() {
    try {
        if (m_states.empty()) {
            auto stdPath = Mod::get()->getResourcesDir() / "Std.gdml";
            if (auto src = SrcFile::from(stdPath)) {
                GEODE_UNWRAP(this->add(src.unwrap()));
            }
            else {
                this->log(Message {
                    .level = Level::Error,
                    .src = m_root,
                    .info = "Unable to find Std.gdml",
                    .range = Range(m_root->getLocation(0)),
                });
            }
            GEODE_UNWRAP(this->add(m_root));
        }
        for (auto& [src, parser] : m_states) {
            (void)parser->m_ast->typecheck(parser);
        }
        if (auto count = this->getErrors().size()) {
            return Err<size_t>(std::move(count));
        }
        return Ok();
    }
    catch(std::exception const& e) {
        this->log(Message {
            .level = Level::Error,
            .src = m_root,
            .info = e.what(),
            .range = Range(m_root->getLocation(0)),
        });
        return Err<size_t>(std::move(this->getErrors().size()));
    }
}

void SharedParser::dispatchLogs() const {
    for (auto& [_, msg] : m_messages) {
        switch (msg.level) {
            default:
            case Level::Info:    log::info("{}", msg.toString()); break;
            case Level::Error:   log::error("{}", msg.toString()); break;
            case Level::Warning: log::warn("{}", msg.toString()); break;
        }
    }
}

void SharedParser::log(Message const& message, size_t level) {
    m_messages.push_back({ level, message });
}

size_t SharedParser::pushLogLevel() {
    return m_rollbackLevel++;
}

void SharedParser::popLogLevel() {
    m_rollbackLevel -= 1;
}

void SharedParser::popMessages(size_t level) {
    ranges::remove(m_messages, [=](std::pair<size_t, Message> const& pair) {
        return pair.first >= level;
    });
}

std::vector<Message> SharedParser::getErrors() const {
    std::vector<Message> errs;
    for (auto& [_, msg] : m_messages) {
        if (msg.level == Level::Error) {
            errs.push_back(msg);
        }
    }
    return errs;
}
