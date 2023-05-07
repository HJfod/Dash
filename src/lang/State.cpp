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

bool ParsedSrc::addExportedType(Type const& type) {TRY_WITH_DEBINFO(
    auto name = type.toString();
    if (m_exportedTypes.contains(name)) {
        return false;
    }
    m_exportedTypes.insert({ name, type });
    return true;
)}

Option<Type> ParsedSrc::getExportedType(Ident const& name) const {TRY_WITH_DEBINFO(
    if (m_exportedTypes.contains(name)) {
        return m_exportedTypes.at(name);
    }
    return None;
)}

Vec<Type> ParsedSrc::getExportedTypes() const {TRY_WITH_DEBINFO(
    Vec<Type> types;
    for (auto& [_, ty] : m_exportedTypes) {
        types.push_back(ty);
    }
    return types;
)}

UnitParser::UnitParser(Parser& parser, Rc<Src> src)
  : m_parser(parser), m_src(src), m_scopes({ Scope() })
{
    this->pushType(Type(VoidType()));
    this->pushType(Type(BoolType()));
    this->pushType(Type(IntType()));
    this->pushType(Type(FloatType()));
    this->pushType(Type(StrType()));
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

void UnitParser::pushType(Type const& type) {TRY_WITH_DEBINFO(
    m_scopes.back().types.insert({ type.toString(), type });
)}

Type* UnitParser::getType(std::string const& name, bool topOnly) {TRY_WITH_DEBINFO(
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
)}

void UnitParser::pushVar(Var const& var) {TRY_WITH_DEBINFO(
    m_scopes.back().vars.insert({ var.name, var });
)}

Var* UnitParser::getVar(std::string const& name, bool topOnly) {TRY_WITH_DEBINFO(
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
)}

void UnitParser::pushScope(bool function) {TRY_WITH_DEBINFO(
    m_scopes.emplace_back().function = function;
)}

void UnitParser::popScope() {TRY_WITH_DEBINFO(
    m_scopes.pop_back();
    if (m_scopes.empty()) {
        throw std::runtime_error("Scope stack is empty");
    }
)}

bool UnitParser::isRootScope() const {
    return m_scopes.size() == 1;
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
