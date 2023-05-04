#include <lang/State.hpp>
#include <lang/Expr.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

State::State() : m_scopes({ Scope() }) {}

void State::pushType(Type const& type) {
    m_scopes.back().types.insert({ type.toString(), type });
}

Type* State::getType(std::string const& name, bool topOnly) {
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

void State::pushVar(Var const& var) {
    m_scopes.back().vars.insert({ var.name, var });
}

Var* State::getVar(std::string const& name, bool topOnly) {
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

void State::pushScope() {
    m_scopes.emplace_back();
}

void State::popScope() {
    m_scopes.pop_back();
    if (m_scopes.empty()) {
        throw std::runtime_error("Internal Compiler Error: Scope stack is empty");
    }
}

Rc<State> State::create() {
    return std::make_shared<State>();
}

ParseResult<> State::parse(Rc<Src> src) {
    auto stream = src->read(shared_from_this());
    GEODE_UNWRAP_INTO(auto ast, AST::pull(stream));
    m_asts[src] = ast;
    log::debug("Succesfully parsed AST for {}", src->getName());
    log::debug("{}", ast->debug());
    log::debug("Typechecking");
    GEODE_UNWRAP(ast->typecheck(shared_from_this()));
    return Ok();
}

void State::dispatchLogs() const {
    for (auto& [_, msg] : m_messages) {
        switch (msg.level) {
            default:
            case Level::Info:    log::info("{}", msg.toString()); break;
            case Level::Error:   log::error("{}", msg.toString()); break;
            case Level::Warning: log::warn("{}", msg.toString()); break;
        }
    }
}

void State::log(Message const& message, size_t level) {
    m_messages.push_back({ level, message });
}

size_t State::pushLogLevel() {
    return m_rollbackLevel++;
}

void State::popLogLevel() {
    m_rollbackLevel -= 1;
}

void State::popMessages(size_t level) {
    ranges::remove(m_messages, [=](std::pair<size_t, Message> const& pair) {
        return pair.first >= level;
    });
}

std::vector<Message> State::getErrors() const {
    std::vector<Message> errs;
    for (auto& [_, msg] : m_messages) {
        if (msg.level == Level::Error) {
            errs.push_back(msg);
        }
    }
    return errs;
}
