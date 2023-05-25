#pragma once

#include "Main.hpp"
#include "Type.hpp"
#include "Src.hpp"

class HotReloadNode;

namespace gdml::lang {
    class AST;
    class UnitParser;
    class Parser;
    class Expr;
    class IdentExpr;
    class Entity;

    struct GDML_DLL Var final {
        IdentPath name;
        Type type;
        bool isExtern;
        Rc<const Expr> decl;
    };

    struct GDML_DLL Fun final {
        IdentPath name;
        FunType type;
        Rc<const Expr> decl;
    };

    struct GDML_DLL Namespace final {
        IdentPath name;
        Map<FullIdentPath, Entity> entities;
        Rc<const Expr> decl;
    };

    class Entity final {
    private:
        std::variant<Var, Fun, Type, Namespace> value;

    public:
        using Value = decltype(value);

        Entity() = default;
        Entity(Value const& value) : value(value) {}
        template <class T>
            requires requires(T const& v) {
                Value(v);
            }
        Entity(T const& value) : value(value) {}

        Option<IdentPath> getName() const;
        Rc<const Expr> getDecl() const;
        Option<Type> getType() const;

        template <class T>
        bool has() const {
            return std::holds_alternative<T>(this->value);
        }

        template <class T>
        T* get() {
            return std::get_if<T>(&this->value);
        }

        template <class T>
        T const* get() const {
            return std::get_if<T>(&this->value);
        }
    };

    class GDML_DLL ParsedSrc final {
    private:
        Rc<Src> m_src;
        Rc<AST> m_ast;
        Map<FullIdentPath, Entity> m_exported;

        friend class UnitParser;

    public:
        ParsedSrc(Rc<Src> src, Rc<AST> ast);

        Rc<AST> getAST() const;

        void addExported(UnitParser& parser, Range const& range, Entity const& type);
        Option<Entity> getExported(FullIdentPath const& name) const;
        Vec<Entity> getAllExported() const;
    };

    class GDML_DLL Scope final {
    private:
        UnitParser& m_parser;
        Map<FullIdentPath, Entity> m_entities;
        bool m_function;

        Scope(bool function, UnitParser& parser);

        friend class UnitParser;

    public:
        void push(Entity const& ent);
        Vec<Entity> getEntities() const;
    };

    class GDML_DLL UnitParser final {
    private:
        Parser& m_parser;
        Vec<Scope> m_scopes;
        Vec<IdentPath> m_namespace;
        Rc<Src> m_src;
        Rc<ParsedSrc> m_parsed;

        FullIdentPath getCurrentNamespace() const;

        UnitParser(Parser& parser, Rc<Src> src);
        UnitParser(UnitParser const&) = delete;
        UnitParser(UnitParser&&) = delete;
        UnitParser& operator=(UnitParser const&) = delete;
        UnitParser& operator=(UnitParser&&) = delete;

        friend class Parser;

    public:
        static Rc<ParsedSrc> parse(Parser& parser, Rc<Src> src);

        Parser& getShared() const;
        Rc<Src> getSrc() const;
        Rc<ParsedSrc> getParsedSrc() const;

        bool verifyCanPush(Rc<IdentExpr> name);
        geode::Result<FullIdentPath> resolve(IdentPath const& name, bool existing);

        void push(Entity const& entity);
        Entity* getEntity(IdentPath const& name, bool topOnly = false);
        Type* getType(IdentPath const& name, bool topOnly = false);
        Var* getVar(IdentPath const& name, bool topOnly = false);
        Fun* getFun(IdentPath const& name, bool topOnly = false);

        template <class T>
        T* get(IdentPath const& name, bool topOnly = false) {
            if (auto ent = this->getEntity(name, topOnly)) {
                return ent->template get<T>();
            }
            return nullptr;
        }

        void pushNamespace(IdentPath const& ns);
        void popNamespace(std::source_location const = std::source_location::current());

        void pushScope(bool function);
        void popScope(std::source_location const = std::source_location::current());
        bool isRootScope() const;
        Scope& scope(size_t depth = 0);
        Vec<Scope> const& getScopes() const;

        template <class... Args>
        Message& log(Range const& range, std::string const& fmt, Args&&... args) {
            auto msg = Message {
                .level = Level::Info,
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = range,
            };
            return this->getShared().log(msg);
        }

        template <class... Args>
        Message& warn(Range const& range, std::string const& fmt, Args&&... args) {
            auto msg = Message {
                .level = Level::Warning,
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = range,
            };
            return this->getShared().log(msg);
        }

        template <class... Args>
        Message& error(Range const& range, std::string const& fmt, Args&&... args) {
            auto msg = Message {
                .level = Level::Error,
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = range,
            };
            return this->getShared().log(msg);
        }
    };

    class GDML_DLL Parser : public cocos2d::CCObject {
    private:
        Rc<Src> m_root;
        Rc<ParsedSrc> m_parsed;
        Vec<cocos2d::CCNode*> m_created;
        Vec<std::pair<size_t, Message>> m_messages;
        // Rollback level starts at 1 because non-rollback messages have 0
        size_t m_rollbackLevel = 1;

        void dispatchLogs() const;

        friend class HotReloadNode;

        Parser(Rc<Src> src);
        Parser(Parser const&) = delete;
        Parser& operator=(Parser const&) = delete;

    public:
        static Parser* create(Rc<Src> src);
        static Parser* create(Path const& file);

        void compile();
        void populate(cocos2d::CCNode* node);

        Message& log(Message const& message, size_t level = 0);
        size_t pushLogLevel();
        void popLogLevel();
        void popMessages(size_t level);
        std::vector<Message> getErrors() const;
    };
}
