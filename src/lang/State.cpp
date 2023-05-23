#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

Option<IdentPath> Entity::getName() const {
    return std::visit(makeVisitor {
        [](Type const& ty) {
            return ty.getName();
        },
        [](auto const& obj) -> Option<IdentPath> {
            return obj.name;
        },
    }, value);
}

Rc<const Expr> Entity::getDecl() const {
    return std::visit(makeVisitor {
        [](Type const& ty) {
            return ty.getDecl();
        },
        [](auto const& obj) {
            return obj.decl;
        },
    }, value);
}

Option<Type> Entity::getType() const {
    return std::visit(makeVisitor {
        [](Type const& ty) -> Option<Type> {
            return ty;
        },
        [](Var const& var) -> Option<Type> {
            return var.type;
        },
        [](Fun const& fun) -> Option<Type> {
            return Type(fun.type, fun.decl);
        },
        [](Namespace const& ns) -> Option<Type> {
            return None;
        },
    }, value);
}

ParsedSrc::ParsedSrc(Rc<Src> src, Rc<AST> ast) : m_src(src), m_ast(ast) {}

Rc<AST> ParsedSrc::getAST() const {
    return m_ast;
}

void ParsedSrc::addExported(UnitParser& parser, Range const& range, Entity const& ent) {
    auto name = ent.getName();
    if (!name) {
        parser.error(range, "Can't export anonymous entities");
        return;
    }
    auto path = parser.resolve(name.value(), false);
    if (!path) {
        parser.error(range, "Can't export an entity in this scope");
        return;
    }
    if (m_exported.contains(path.unwrap())) {
        parser.error(range, "\"{}\" has already been exported", path.unwrap());
        return;
    }
    m_exported.insert({ path.unwrap(), ent });
}

Option<Entity> ParsedSrc::getExported(FullIdentPath const& name) const {
    if (m_exported.contains(name)) {
        return m_exported.at(name);
    }
    return None;
}

Vec<Entity> ParsedSrc::getAllExported() const {
    Vec<Entity> types;
    for (auto& [_, ty] : m_exported) {
        types.push_back(ty);
    }
    return types;
}

Scope::Scope(Option<IdentPath> const& name, bool function, UnitParser& parser)
    : m_name(name), m_function(function), m_parser(parser) {}

void Scope::push(Entity const& ent) {
    if (auto name = ent.getName()) {
        if (auto path = m_parser.resolve(name.value(), false)) {
            this->m_entities.insert({ path.unwrap(), ent });
        }
    }
}

Vec<Entity> Scope::getEntities() const {
    return map::values(m_entities);
}

UnitParser::UnitParser(Parser& parser, Rc<Src> src)
  : m_parser(parser), m_src(src), m_scopes({ Scope(None, false, *this) })
{
    this->push(Type(Primitive::Void));
    this->push(Type(Primitive::Bool));
    this->push(Type(Primitive::Int));
    this->push(Type(Primitive::Float));
    this->push(Type(Primitive::Str));
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
    auto path = this->resolve(name->path, false);
    if (!path) {
        this->error(name->range, "{}", path.unwrapErr());
        return false;
    }
    if (m_scopes.back().m_entities.contains(path.unwrap())) {
        this->error(name->range, "Type or variable \"{}\" already exists in this scope", name->path);
        return false;
    }
    return true;
}

Result<FullIdentPath> UnitParser::resolve(IdentPath const& name, bool existing) {
    if (!existing) {
        // Absolute paths are resolved as is
        if (name.absolute) {
            return Ok(FullIdentPath(name));
        }
        // If the name contains a parent path (e.g. `a::b` in `a::b::c`) then that 
        // must already exist
        else if (auto parent = name.getParent()) {
            GEODE_UNWRAP_INTO(auto target, this->resolve(parent.value(), true));
            return Ok(target.join(name.name));
        }
        // Otherwise just append the name to the end of the current scope
        else {
            auto path = FullIdentPath();
            for (auto& scope : m_scopes) {
                if (scope.m_name) {
                    path = path.join(scope.m_name.value());
                }
            }
            path.join(name);
            return Ok(path);
        }
    }
    else {
    }
    for (auto& scope : ranges::reverse(m_scopes)) {
        for (auto& [path, entity] : scope.m_entities) {
            if (auto resolved = path.resolve(name, existing)) {
                return Ok(resolved.value());
            }
        }
    }
    return Err("Unknown namespace \"{}\"", fmt::join(name.path, "::"));
}

void UnitParser::push(Entity const& entity) {
    m_scopes.back().push(entity);
}

Entity* UnitParser::getEntity(IdentPath const& name, bool topOnly) {
    if (auto path = this->resolve(name, true)) {
        // Prefer topmost scope
        for (auto& scope : ranges::reverse(m_scopes)) {
            if (scope.m_entities.contains(path.unwrap())) {
                return &scope.m_entities.at(path.unwrap());
            }
            if (topOnly) {
                break;
            }
        }
    }
    return nullptr;
}

Type* UnitParser::getType(IdentPath const& name, bool topOnly) {
    return this->template get<Type>(name, topOnly);
}

Var* UnitParser::getVar(IdentPath const& name, bool topOnly) {
    return this->template get<Var>(name, topOnly);
}

Fun* UnitParser::getFun(IdentPath const& name, bool topOnly) {
    return this->template get<Fun>(name, topOnly);
}

void UnitParser::pushScope(Option<IdentPath> const& name, bool function) {
    m_scopes.push_back(Scope(name, function, *this));
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

Vec<Scope> const& UnitParser::getScopes() const {
    return m_scopes;
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
            .info = src.unwrapErr(),
            .range = None,
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
    size_t errorCount = 0;
    size_t warnCount = 0;
    for (auto& [_, msg] : m_messages) {
        switch (msg.level) {
            default:
            case Level::Info: {
                log::info("{}", msg.toString());
            } break;

            case Level::Error: {
                log::error("{}", msg.toString());
                errorCount += 1;
            } break;

            case Level::Warning: { 
                log::warn("{}", msg.toString());
                warnCount += 1;
            } break;
        }
    }
    log::info("Finished with {} errors and {} warnings", errorCount, warnCount);
}

Message& Parser::log(Message const& message, size_t level) {
    m_messages.push_back({ level, message });
    return m_messages.back().second;
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
