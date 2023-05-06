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
        Map<Ident, Type> m_exportedTypes;

        friend class UnitParser;

    public:
        ParsedSrc(Rc<Src> src, Rc<AST> ast);

        Rc<AST> getAST() const;

        bool addExportedType(Type const& type);
        Option<Type> getExportedType(Ident const& name) const;
        Vec<Type> getExportedTypes() const;
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
        Rc<Src> m_src;
        Rc<ParsedSrc> m_parsed;

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

        void pushType(Type const& type);
        Type* getType(std::string const& name, bool topOnly = false);

        void pushVar(Var const& var);
        Var* getVar(std::string const& name, bool topOnly = false);

        void pushScope();
        void popScope();
        bool isRootScope() const;

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
        void error(Range const& range, std::string const& fmt, Args&&... args) {
            auto msg = Message {
                .level = Level::Error,
                .src = range.start.src,
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = range,
            };
            this->getShared().log(msg);
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

        void log(Message const& message, size_t level = 0);
        size_t pushLogLevel();
        void popLogLevel();
        void popMessages(size_t level);
        std::vector<Message> getErrors() const;
    };
}
