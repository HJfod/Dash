#pragma once

#include "Main.hpp"

namespace gdml::lang {
    class Src;
    class Rollback;
    class SrcParser;
    struct Token;

    struct GDML_DLL Location {
        Rc<Src> src;
        size_t line;
        size_t column;
        size_t offset;

        bool operator==(Location const& other) const;
        std::string toString() const;
    };

    struct GDML_DLL Range {
        Location start;
        Location end;
        
        Range(Location const& pos) : start(pos), end(pos) {}
        Range(Location const& start, Location const& end)
        : start(start), end(end) {}

        std::string toString() const;
    };

    enum class Level {
        Info,
        Warning,
        Error,
    };

    struct GDML_DLL Message {
        Level level;
        Rc<Src> src;
        std::string info;
        Range range;

        std::string toString() const;
    };

    class GDML_DLL Stream final {
    private:
        Rc<Src> m_file;
        Rc<SrcParser> m_state;
        size_t m_position = 0;
        size_t m_debugTickCounter = 0;
        Token* m_lastToken = nullptr;

        Stream(Rc<Src> file, Rc<SrcParser> state);

        friend class SrcFile;
        friend class Rollback;

    public:
        char peek() const;
        std::string peek(size_t count) const;
        char next();
        bool eof() const;
        void navigate(size_t offset);
        void debugTick(std::source_location const loc = std::source_location::current());
        void setLastToken(Token const& token);
        Option<Token> last() const;

        Stream(Stream const&) = delete;
        Stream(Stream&&) = delete;
        ~Stream();

        Rc<Src> src() const;
        size_t offset() const;
        Location location() const;
        Rc<SrcParser> state() const;
    };

    class GDML_DLL Src {
    public:
        virtual std::string getName() const = 0;

        virtual Stream read(Rc<SrcParser> state) = 0;

        virtual size_t size() const = 0;
        virtual char at(size_t offset) const = 0;
        virtual std::string from(size_t offset, size_t count) const = 0;
        virtual Location getLocation(size_t offset) = 0;
        virtual std::string getUnderlined(Range const& range) const = 0;

        virtual bool onChanged(std::function<void(Rc<Src>)> callback) = 0;

        virtual ~Src() = default;
    };

    class GDML_DLL SrcFile final : public Src, public std::enable_shared_from_this<SrcFile> {
    private:
        ghc::filesystem::path m_path;
        std::string m_data;
        std::unique_ptr<geode::EventListener<geode::utils::file::FileWatchFilter>> m_listener;

    public:
        SrcFile(ghc::filesystem::path const& path, std::string const& data);
        SrcFile(SrcFile const&) = delete;
        virtual ~SrcFile();

        static geode::Result<Rc<SrcFile>> from(ghc::filesystem::path const& path);

        std::string getName() const override;
        size_t size() const override;
        char at(size_t offset) const override;
        std::string from(size_t offset, size_t count) const override;
        Stream read(Rc<SrcParser> state) override;
        std::string getUnderlined(Range const& range) const override;
        bool onChanged(std::function<void(Rc<Src>)> callback) override;

        ghc::filesystem::path getPath() const;
        std::string const& getData() const;

        Location getLocation(size_t offset) override;
    };

    struct GDML_DLL Rollback final {
    private:
        Stream& m_stream;
        bool m_commit = false;
        size_t m_offset;
        size_t m_msgLevel;

    public:
        Rollback(Stream& stream, std::source_location const loc = std::source_location::current());
        ~Rollback();

        void clearMessages();
        void commit();

        template <class E, class... Args>
        ExprResult<E> commit(Args&&... args) {
            this->commit();
            return geode::Ok(std::make_shared<E>(
                std::forward<Args>(args)...,
                Range(m_stream.src()->getLocation(m_offset), m_stream.location())
            ));
        }

        geode::impl::Failure<size_t> error(Message const& message);

        template <class... Args>
        geode::impl::Failure<size_t> error(std::string const& fmt, Args&&... args) {
            return this->error(Message {
                .level = Level::Error,
                .src = m_stream.src(),
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = Range(m_stream.src()->getLocation(m_offset), m_stream.location())
            });
        }

        template <class... Args>
        geode::impl::Failure<size_t> errorLastToken(std::string const& fmt, Args&&... args) {
            return this->error(Message {
                .level = Level::Error,
                .src = m_stream.src(),
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = Range(m_stream.src()->getLocation(m_offset), m_stream.src()->getLocation(m_offset))
            });
        }

        template <class... Args>
        geode::impl::Failure<size_t> errorNextToken(std::string const& fmt, Args&&... args) {
            Token::skipToNext(m_stream);
            return this->error(Message {
                .level = Level::Error,
                .src = m_stream.src(),
                .info = fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...),
                .range = Range(m_stream.location(), m_stream.location())
            });
        }
    };
}
