#pragma once

#include <Geode/DefaultInclude.hpp>

using namespace geode::prelude;

template <class T>
using Rc = std::shared_ptr<T>;

class SrcFile;

class Stream final {
private:
    Rc<SrcFile> m_file;
    size_t m_position = 0;

    Stream(Rc<SrcFile> file);

    friend class SrcFile;

public:
    char next();
};

class SrcFile final : public std::enable_shared_from_this<SrcFile> {
private:
    ghc::filesystem::path m_path;
    std::string m_data;

public:
    SrcFile(ghc::filesystem::path const& path, std::string const& data);

    static Result<Rc<SrcFile>> from(ghc::filesystem::path const& path);

    Stream read();

    ghc::filesystem::path getPath() const;
    std::string const& getData() const;
};

struct Location {
    Rc<SrcFile> file;
    size_t line;
    size_t column;
};

struct Range {
    Location start;
    Location end;
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
