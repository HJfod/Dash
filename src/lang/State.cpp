#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ParsedSrc::ParsedSrc(Rc<Src> src, Rc<AST> ast) : m_src(src), m_ast(ast) {}

Rc<AST> ParsedSrc::getAST() const {
    return m_ast;
}

bool ParsedSrc::addExportedType(Type const& type) {
    auto name = type.getName();
    if (!name || m_exportedTypes.contains(name.value())) {
        return false;
    }
    m_exportedTypes.insert({ name.value(), type });
    return true;
}

Option<Type> ParsedSrc::getExportedType(IdentPath const& name) const {
    if (m_exportedTypes.contains(name)) {
        return m_exportedTypes.at(name);
    }
    return None;
}

Vec<Type> ParsedSrc::getExportedTypes() const {
    Vec<Type> types;
    for (auto& [_, ty] : m_exportedTypes) {
        types.push_back(ty);
    }
    return types;
}

void Scope::pushType(Type const& type) {
    if (auto name = type.getName()) {
        this->types.insert({ name.value(), type });
    }
}

void Scope::pushVar(Var const& var) {
    this->vars.insert({ var.name, var });
}

UnitParser::UnitParser(Parser& parser, Rc<Src> src)
  : m_parser(parser), m_src(src), m_scopes({ Scope() })
{
    this->pushType(Primitive::Void);
    this->pushType(Primitive::Bool);
    this->pushType(Primitive::Int);
    this->pushType(Primitive::Float);
    this->pushType(Primitive::Str);
}

Rc<ParsedSrc> UnitParser::parse(Parser& shared, Rc<Src> src) {
    auto unit = UnitParser(shared, src);
    auto stream = src->read(unit);
    auto ast = AST::pull(stream).unwrapOr(nullptr);
    auto parsed = std::make_shared<ParsedSrc>(src, ast);
    if (ast) {
        log::debug("Succesfully parsed AST for {}", src->getName());
        log::debug("{}", ast->debug());
        unit.m_parsed = parsed;
        ast->typecheck(unit);
    }
    return parsed;
}

Parser& UnitParser::getShared() const {
    return m_parser;
}

Rc<Src> UnitParser::getSrc() const {
    return m_src;
}

Rc<ParsedSrc> UnitParser::getParsedSrc() const {
    return m_parsed;
}

bool UnitParser::verifyCanPush(Rc<IdentExpr> name) {
    if (m_scopes.back().types.contains(name->path) || m_scopes.back().vars.contains(name->path)) {
        this->error(name->range, "Type \"{}\" already exists in this scope", name->path);
        return false;
    }
    return true;
}

void UnitParser::pushType(Type const& type) {
    m_scopes.back().pushType(type);
}

Type* UnitParser::getType(IdentPath const& name, bool topOnly) {
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
    m_scopes.back().pushVar(var);
}

Var* UnitParser::getVar(IdentPath const& name, bool topOnly) {
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

void UnitParser::pushNamespace(Ident const& ns) {
    m_namespace.push_back(ns);
}

void UnitParser::popNamespace(std::source_location const loc) {
    if (m_namespace.empty()) {
        throw std::runtime_error(fmt::format("Namespace stack is empty (tried to pop from {})", loc));
    }
    m_namespace.pop_back();
}

void UnitParser::pushScope(bool function) {
    m_scopes.emplace_back().function = function;
}

void UnitParser::popScope(std::source_location const loc) {
    m_scopes.pop_back();
    if (m_scopes.empty()) {
        throw std::runtime_error(fmt::format("Scope stack is empty (tried to pop from {})", loc));
    }
}

bool UnitParser::isRootScope() const {
    return m_scopes.size() == 1;
}

Scope& UnitParser::scope(size_t depth) {
    try {
        return m_scopes.at(m_scopes.size() - depth - 1);
    }
    catch(...) {
        throw std::runtime_error(fmt::format(
            "Attempted to access scope past bounds ({}) at depth {}",
            m_scopes.size(), depth
        ));
    }
}

Parser::Parser(Rc<Src> src) : m_root(src) {
    this->autorelease();
}

Parser* Parser::create(Rc<Src> src) {
    return new Parser(src);
}

Parser* Parser::create(Path const& file) {
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

void Parser::compile() {
    if (!m_root) return;
    try {
        m_parsed = UnitParser::parse(*this, m_root);
    }
    catch(std::exception const& e) {
        this->log(Message {
            .level = Level::Error,
            .src = m_root,
            .info = "Internal Compiler Error: " + std::string(e.what()),
            .range = Range(m_root->getLocation(0)),
        });
    }
}

void Parser::populate(CCNode* node) {
    for (auto& created : m_created) {
        created->removeFromParent();
    }
    m_created.clear();
    this->dispatchLogs();
    if (!this->getErrors().empty()) {
        auto label = CCLabelBMFont::create(
            "There were errors loading GDML\n(See console output)",
            "bigFont.fnt"
        );
        label->setPosition(node->getContentSize() / 2);
        limitNodeSize(label, node->getContentSize(), 1.f, .1f);
        m_created.push_back(label);
    }
    for (auto& created : m_created) {
        node->addChild(created);
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
