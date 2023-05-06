#pragma once

#include "Main.hpp"
#include "Type.hpp"
#include "Src.hpp"

class HotReloadNode;

namespace gdml::lang {
    class AST;
    class UnitParser;
    class Parser;

    class GDML_DLL ParsedSrc final {
    private:
        Rc<Src> m_src;
        Rc<AST> m_ast;
    
        ParsedSrc(Rc<Src> src, Rc<AST> ast);

        friend class UnitParser;

    public:
        Rc<AST> getAST() const;
    };

    struct GDML_DLL Var final {
        std::string name;
        Type type;
    };

    struct GDML_DLL Scope final {
        Map<std::string, Type> types;
        Map<std::string, Var> vars;
    };

    class GDML_DLL UnitParser final {
    private:
        Parser& m_parser;
        Vec<Scope> m_scopes;

        UnitParser(Parser& parser);
        UnitParser(UnitParser const&) = delete;
        UnitParser(UnitParser&&) = delete;
        UnitParser& operator=(UnitParser const&) = delete;
        UnitParser& operator=(UnitParser&&) = delete;

        friend class Parser;

    public:
        static ParseResult<ParsedSrc> parse(Parser& parser, Rc<Src> src);
        static ParseResult<> typecheck(Parser& parser, ParsedSrc const& src);

        Parser& getShared() const;

        void pushType(Type const& type);
        Type* getType(std::string const& name, bool topOnly = false);

        void pushVar(Var const& var);
        Var* getVar(std::string const& name, bool topOnly = false);

        void pushScope();
        void popScope();

        template <class... Args>
        void warn(Range const& range, std::string const& fmt, Args&&... args) {
            auto msg = Message {
                .level = Level::Warning,
                .src = range.start.src,
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = range,
            };
            this->getShared().log(msg);
        }

        template <class... Args>
        geode::impl::Failure<size_t> error(Range const& range, std::string const& fmt, Args&&... args) {
            auto msg = Message {
                .level = Level::Error,
                .src = range.start.src,
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = range,
            };
            this->getShared().log(msg);
            return geode::Err(this->getShared().getErrors().size());
        }
    };

    class GDML_DLL Parser : public cocos2d::CCObject {
    private:
        Rc<Src> m_root;
        Map<Rc<Src>, Owned<geode::EventListenerProtocol>> m_watchers;
        Map<Rc<Src>, ParsedSrc> m_srcs;
        Vec<std::pair<size_t, Message>> m_messages;
        // Rollback level starts at 1 because non-rollback messages have 0
        size_t m_rollbackLevel = 1;

        void add(Rc<Src> src);
        void dispatchLogs() const;

        friend class HotReloadNode;

        Parser(Rc<Src> src);
        Parser(Parser const&) = delete;
        Parser& operator=(Parser const&) = delete;

    public:
        static Parser* create(Rc<Src> src);
        static Parser* create(ghc::filesystem::path const& file);

        void compile();
        void populate(cocos2d::CCNode* node);

        void log(Message const& message, size_t level = 0);
        size_t pushLogLevel();
        void popLogLevel();
        void popMessages(size_t level);
        std::vector<Message> getErrors() const;
    };
}
