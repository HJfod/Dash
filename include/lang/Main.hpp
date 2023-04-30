#pragma once

#include <Geode/DefaultInclude.hpp>
#include <Geode/utils/file.hpp>

namespace gdml {
    // this code has not been approved by the Rust foundation

    template <class T>
    using Box = std::unique_ptr<T>;

    template <class T>
    using Rc = std::shared_ptr<T>;

    template <class T>
    using Option = std::optional<T>;
    constexpr auto None = std::nullopt;

    template <class T>
    using Vec = std::vector<T>;
}

namespace gdml::lang {
    class Src;

    struct Location {
        Rc<Src> src;
        size_t line;
        size_t column;
        size_t offset;

        bool operator==(Location const& other) const;
        std::string toString() const;
    };

    struct Range {
        Location start;
        Location end;
        Range() = default;
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

    struct Message {
        Level level;
        Rc<Src> src;
        std::string info;
        Range range;

        std::string toString() const;
    };

    class Stream final {
    private:
        Rc<Src> m_file;
        size_t m_position = 0;
        size_t m_debugTickCounter = 0;
        std::vector<Message> m_messages;

        Stream(Rc<Src> file);

        friend class SrcFile;

    public:
        char peek() const;
        std::string peek(size_t count) const;
        char next();
        bool eof() const;
        void navigate(size_t offset);
        void debugTick();

        Rc<Src> src() const;
        size_t offset() const;
        Location location() const;
        void log(Message const& message);
        std::vector<Message> errors() const;
    };

    class Src {
    public:
        virtual std::string getName() const = 0;

        virtual Stream read() = 0;

        virtual size_t size() const = 0;
        virtual char at(size_t offset) const = 0;
        virtual std::string from(size_t offset, size_t count) const = 0;
        virtual Location getLocation(size_t offset) = 0;

        virtual bool onChanged(std::function<void(Rc<Src>)> callback) = 0;

        virtual ~Src() = default;
    };

    class SrcFile final : public Src, public std::enable_shared_from_this<SrcFile> {
    private:
        ghc::filesystem::path m_path;
        std::string m_data;
        Box<geode::EventListener<geode::utils::file::FileWatchFilter>> m_listener;

    public:
        SrcFile(ghc::filesystem::path const& path, std::string const& data);
        SrcFile(SrcFile const&) = delete;
        virtual ~SrcFile();

        static geode::Result<Rc<SrcFile>> from(ghc::filesystem::path const& path);

        std::string getName() const override;
        size_t size() const override;
        char at(size_t offset) const override;
        std::string from(size_t offset, size_t count) const override;
        Stream read() override;
        bool onChanged(std::function<void(Rc<Src>)> callback) override;

        ghc::filesystem::path getPath() const;
        std::string const& getData() const;

        Location getLocation(size_t offset) override;
    };
}
