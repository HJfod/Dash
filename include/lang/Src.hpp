#pragma once

#include "Main.hpp"

namespace gdml::lang {
    class Src;
    class Rollback;
    class UnitParser;
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
        
        Range() : Range(nullptr) {}
        Range(Rc<Src> src) : Range(Location {
            .src = src,
            .line = 0,
            .column = 0,
            .offset = 0,
        }) {}
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
        UnitParser& m_state;
        size_t m_position = 0;
        size_t m_debugTickCounter = 0;
        std::unique_ptr<Token> m_lastToken = nullptr;

        Stream(Rc<Src> file, UnitParser& state);

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
        UnitParser& state() const;

        template <class E, class... Args>
        Rc<E> make(Args&&... args) {
            return std::make_shared<E>(std::forward<Args>(args)..., Range(this->location()));
        }
    };

    class GDML_DLL Src {
    public:
        virtual std::string getName() const = 0;

        virtual Stream read(UnitParser& state) = 0;

        virtual size_t size() const = 0;
        virtual char at(size_t offset) const = 0;
        virtual std::string from(size_t offset, size_t count) const = 0;
        virtual Location getLocation(size_t offset) = 0;
        virtual std::string getUnderlined(Range const& range) const = 0;

        virtual Path getSearchDir() const = 0;

        virtual ~Src() = default;
    };

    class GDML_DLL SrcFile final : public Src, public std::enable_shared_from_this<SrcFile> {
    private:
        Path m_path;
        std::string m_data;

    public:
        SrcFile(Path const& path, std::string const& data);
        SrcFile(SrcFile const&) = delete;
        virtual ~SrcFile();

        static geode::Result<Rc<SrcFile>> from(Path const& path);

        std::string getName() const override;
        size_t size() const override;
        char at(size_t offset) const override;
        std::string from(size_t offset, size_t count) const override;
        Stream read(UnitParser& state) override;
        std::string getUnderlined(Range const& range) const override;

        Path getSearchDir() const override;
        
        Path getPath() const;
        std::string const& getData() const;

        Location getLocation(size_t offset) override;
    };

    struct GDML_DLL Rollback final {
    private:
        Stream& m_stream;
        bool m_commit = false;
        size_t m_offset;
        size_t m_msgLevel;
        std::unique_ptr<Token> m_lastToken = nullptr;

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
