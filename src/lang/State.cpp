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
        [](Namespace const&) -> Option<Type> {
            return None;
        },
        [](ScopeEntity const&) -> Option<Type> {
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

Scope::Scope(Option<FullIdentPath> const& name, bool function, UnitParser& parser)
    : m_name(name), m_function(function), m_parser(parser)
{}

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

void UnitParser::pushAsOp(Primitive from, Primitive into) {
    // msvc straight up hangs if i don't put this in a variable first
    auto ty = FunType {
        .name = IdentPath(asFunName(from, into)),
        .params = {
            ParamType {
                .name = "a",
                .type = from,
            },
        },
        .retType = Type(into),
        .isExtern = true,
    };
    this->push(Fun {
        .name = IdentPath(asFunName(from, into)),
        .type = ty,
        .decl = nullptr,
    });
}

void UnitParser::pushUnOp(Op op, Primitive a, Primitive ret) {
    // msvc straight up hangs if i don't put this in a variable first
    auto ty = FunType {
        .name = IdentPath(opFunName(op, a)),
        .params = {
            ParamType {
                .name = "a",
                .type = a,
            },
        },
        .retType = Type(ret),
        .isExtern = true,
    };
    this->push(Fun {
        .name = IdentPath(opFunName(op, a)),
        .type = ty,
        .decl = nullptr,
    });
}

void UnitParser::pushBinOp(Op op, Primitive t) {
    return this->pushBinOp(op, t, t, t);
}

void UnitParser::pushBinOp(Op op, Primitive a, Primitive b, Primitive ret) {
    // msvc straight up hangs if i don't put this in a variable first
    auto ty = FunType {
        .name = IdentPath(opFunName(op, a, b)),
        .params = {
            ParamType {
                .name = "a",
                .type = a,
            },
            ParamType {
                .name = "b",
                .type = b,
            },
        },
        .retType = Type(ret),
        .isExtern = true,
    };
    this->push(Fun {
        .name = IdentPath(opFunName(op, a, b)),
        .type = ty,
        .decl = nullptr,
    });
}

UnitParser::UnitParser(Parser& parser, Rc<Src> src)
  : m_parser(parser), m_src(src), m_scopes({ Scope(None, false, *this) })
{
    this->push(Type(Primitive::Void));
    this->push(Type(Primitive::Bool));
    this->push(Type(Primitive::Int));
    this->push(Type(Primitive::Float));
    this->push(Type(Primitive::Str));

    using enum gdml::lang::Primitive;

    this->pushBinOp(Op::Add, Int);
    this->pushBinOp(Op::Sub, Int);
    this->pushBinOp(Op::Mul, Int);
    this->pushBinOp(Op::Div, Int);
    this->pushBinOp(Op::Mod, Int);
    this->pushBinOp(Op::Eq, Int, Int, Bool);
    this->pushBinOp(Op::Less, Int, Int, Bool);
    this->pushBinOp(Op::More, Int, Int, Bool);
    this->pushUnOp(Op::Not, Int, Bool);
    this->pushAsOp(Int, Float);
    this->pushAsOp(Int, Str);
    this->pushAsOp(Int, Bool);

    this->pushBinOp(Op::Add, Float);
    this->pushBinOp(Op::Sub, Float);
    this->pushBinOp(Op::Mul, Float);
    this->pushBinOp(Op::Div, Float);
    this->pushBinOp(Op::Mod, Float);
    this->pushBinOp(Op::Eq, Float, Float, Bool);
    this->pushBinOp(Op::Less, Float, Float, Bool);
    this->pushBinOp(Op::More, Float, Float, Bool);
    this->pushUnOp(Op::Not, Float, Bool);
    this->pushAsOp(Float, Int);
    this->pushAsOp(Float, Str);
    this->pushAsOp(Float, Bool);

    this->pushBinOp(Op::Eq, Bool);
    this->pushUnOp(Op::Not, Bool, Bool);
    this->pushAsOp(Bool, Int);
    this->pushAsOp(Bool, Str);

    this->pushBinOp(Op::Add, Str);
    this->pushBinOp(Op::Eq, Str);

    // Allow multiplying and dividing floats and ints without casting
    this->pushBinOp(Op::Mul, Int, Float, Float);
    this->pushBinOp(Op::Div, Int, Float, Float);
    this->pushBinOp(Op::Mul, Float, Int, Float);
    this->pushBinOp(Op::Div, Float, Int, Float);
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

bool UnitParser::verifyCanPush(Range const& range, IdentPath const& name) {
    auto path = this->resolve(name, false);
    if (!path) {
        this->error(range, "{}", path.unwrapErr());
        return false;
    }
    if (m_scopes.back().m_entities.contains(path.unwrap())) {
        this->error(range, "Type or variable \"{}\" already exists in this scope", path.unwrap());
        return false;
    }
    return true;
}

bool UnitParser::verifyCanPush(Rc<IdentExpr> name) {
    return this->verifyCanPush(name->range, name->path);
}

FullIdentPath UnitParser::getCurrentNamespace() const {
    auto cur = FullIdentPath();
    for (auto& ns : m_namespace) {
        cur = cur.enter(ns);
    }
    return cur;
}

Result<FullIdentPath> UnitParser::resolve(IdentPath const& name, bool existing) {
    auto toFind = name;
    if (!existing) {
        if (auto parent = toFind.getParent()) {
            toFind = parent.value();
        }
        else {
            return Ok(FullIdentPath(getCurrentNamespace().enter(name)));
        }
    }
    auto testParent = getCurrentNamespace();
    while (!testParent.path.empty()) {
        auto full = testParent.enter(toFind);
        for (auto& scope : ranges::reverse(m_scopes)) {
            if (scope.m_entities.contains(full)) {
                return Ok(existing ? full : full.join(name.name));
            }
        }
        testParent.path.pop_back();
    }
    return Err("Unknown entity \"{}\"", name.toString());
}

void UnitParser::push(Entity const& entity) {
    m_scopes.back().push(entity);
}

Entity* UnitParser::getEntity(FullIdentPath const& name, bool topOnly) {
    // Prefer topmost scope
    for (auto& scope : ranges::reverse(m_scopes)) {
        if (scope.m_entities.contains(name)) {
            return &scope.m_entities.at(name);
        }
        if (topOnly) {
            break;
        }
    }
    return nullptr;
}

Entity* UnitParser::getEntity(IdentPath const& name, bool topOnly) {
    if (auto path = this->resolve(name, true)) {
        return this->getEntity(path.unwrap());
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

void UnitParser::pushNamespace(IdentPath const& ns) {
    m_namespace.push_back(ns);
}

void UnitParser::popNamespace(std::source_location const loc) {
    if (m_namespace.empty()) {
        throw std::runtime_error(fmt::format("Namespace stack is empty (tried to pop from {})", loc));
    }
    m_namespace.pop_back();
}

void UnitParser::pushScope(Option<Rc<IdentExpr>> const& name, bool function) {
    Option<FullIdentPath> full;
    if (name) {
        full = this->resolve(name.value()->path, false).ok();
    }
    m_scopes.push_back(Scope(full, function, *this));
}

Option<Type> UnitParser::popScope(std::source_location const loc) {
    Option<Type> ret;
    if (m_retInfo) {
        bool breakHere = false;
        if (m_retInfo.value().from) {
            if (auto full = this->resolve(m_retInfo.value().from.value(), true)) {
                if (full == m_scopes.back().m_name) {
                    breakHere = true;
                }
            }
        }
        else if (m_retInfo.value().function && m_scopes.back().m_function) {
            breakHere = true;
        }
        if (breakHere) {
            ret = m_retInfo.value().returnType;
            m_retInfo = None;
        }
    }
    m_scopes.pop_back();
    if (m_scopes.empty()) {
        throw std::runtime_error(fmt::format("Scope stack is empty (popped from {})", loc));
    }
    return ret;
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

void UnitParser::setReturnInfo(Option<IdentPath> const& from, bool function, Type const& retType) {
    if (!m_retInfo) {
        m_retInfo = ReturnInfo {
            .from = from,
            .function = function,
            .returnType = retType,
        };
    }
}

bool UnitParser::isReturning() const {
    return m_retInfo.has_value();
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
