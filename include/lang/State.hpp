#pragma once

#include "Main.hpp"
#include "Type.hpp"
#include "Src.hpp"

namespace gdml::lang {
    class AST;

    struct GDML_DLL Var final {
        std::string name;
        Type type;
    };

    struct GDML_DLL Scope final {
        Map<std::string, Type> types;
        Map<std::string, Var> vars;
    };

    class GDML_DLL State final : public std::enable_shared_from_this<State> {
    private:
        Vec<Scope> m_scopes;
        Vec<std::pair<size_t, Message>> m_messages;
        // Rollback level starts at 1 because non-rollback messages have 0
        size_t m_rollbackLevel = 1;
        Map<Rc<Src>, Rc<AST>> m_asts;

    public:
        State();
        static Rc<State> create();

        ParseResult<> parse(Rc<Src> src);

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
            this->log(msg);
        }

        template <class... Args>
        geode::impl::Failure<size_t> error(Range const& range, std::string const& fmt, Args&&... args) {
            auto msg = Message {
                .level = Level::Error,
                .src = range.start.src,
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = range,
            };
            this->log(msg);
            return geode::Err(this->getErrors().size());
        }

        void log(Message const& message, size_t level = 0);
        void dispatchLogs() const;
        size_t pushLogLevel();
        void popLogLevel();
        void popMessages(size_t level);
        std::vector<Message> getErrors() const;
    };
}
