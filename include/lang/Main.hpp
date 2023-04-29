#pragma once

#include <Geode/DefaultInclude.hpp>

namespace gdml::lang {
    // this code has not been approved by the Rust foundation

    template <class T>
    using Rc = std::shared_ptr<T>;

    template <class T>
    using Option = std::optional<T>;
    constexpr auto None = std::nullopt;

    template <class T>
    using Vec = std::vector<T>;

    class SrcFile;

    struct Location {
        Rc<SrcFile> file;
        size_t line;
        size_t column;
        size_t offset;
    };

    struct Range {
        Location start;
        Location end;
        Range() = default;
        Range(Location const& pos) : start(pos), end(pos) {}
        Range(Location const& start, Location const& end)
        : start(start), end(end) {}
    };

    enum class Level {
        Info,
        Warning,
        Error,
    };

    struct Message {
        Level level;
        Rc<SrcFile> src;
        std::string info;
        Range range;
    };

    class Stream final {
    private:
        Rc<SrcFile> m_file;
        size_t m_position = 0;
        size_t m_debugTickCounter = 0;
        std::vector<Message> m_messages;

        Stream(Rc<SrcFile> file);

        friend class SrcFile;

    public:
        char peek() const;
        std::string peek(size_t count) const;
        char next();
        bool eof() const;
        void navigate(size_t offset);
        void debugTick();

        Rc<SrcFile> src() const;
        size_t offset() const;
        Location location() const;
        void log(Message const& message);
    };

    class SrcFile final : public std::enable_shared_from_this<SrcFile> {
    private:
        ghc::filesystem::path m_path;
        std::string m_data;

    public:
        SrcFile(ghc::filesystem::path const& path, std::string const& data);

        static geode::Result<Rc<SrcFile>> from(ghc::filesystem::path const& path);

        Stream read();

        ghc::filesystem::path getPath() const;
        std::string const& getData() const;

        Location getLocation(size_t offset);
    };
}
