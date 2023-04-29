#include <lang/Main.hpp>
#include <Geode/utils/file.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

SrcFile::SrcFile(ghc::filesystem::path const& path, std::string const& data)
  : m_path(path), m_data(data) {} 

Result<Rc<SrcFile>> SrcFile::from(ghc::filesystem::path const& path) {
    GEODE_UNWRAP_INTO(auto data, file::readString(path));
    return Ok(std::make_shared<SrcFile>(path, data));
}

Stream::Stream(Rc<SrcFile> file) : m_file(file) {}

char Stream::peek() const {
    if (m_file->getData().size() < m_position) {
        return m_file->getData()[m_position];
    }
    return '\0';
}

std::string Stream::peek(size_t count) const {
    if (m_file->getData().size() < m_position + count) {
        return m_file->getData().substr(m_position, count);
    }
    return "";
}

char Stream::next() {
    if (m_file->getData().size() < m_position) {
        return m_file->getData()[m_position++];
    }
    return '\0';
}

bool Stream::eof() const {
    return m_file->getData().size() > m_position;
}

void Stream::navigate(size_t offset) {
    m_position = offset;
}

Rc<SrcFile> Stream::src() const {
    return m_file;
}

size_t Stream::offset() const {
    return m_position;
}

Location Stream::location() const {
    return m_file->getLocation(m_position);
}

void Stream::debugTick() {
    if (m_debugTickCounter++ > 10000000) {
        throw std::runtime_error(
            "Internal Compiler Error: Debug tick level exceeded 10000000 limit. "
            "This implies that the compiler ended up in an infinite loop - "
            "please report this bug!"
        );
    }
}

void Stream::log(Message const& message) {
    m_messages.push_back(message);
}

Stream SrcFile::read() {
    return Stream(shared_from_this());
}

ghc::filesystem::path SrcFile::getPath() const {
    return m_path;
}

std::string const& SrcFile::getData() const {
    return m_data;
}

Location SrcFile::getLocation(size_t offset) {
    size_t i = 0;
    size_t line = 0;
    size_t col = 0;
    for (auto& ch : m_data) {
        if (ch == '\n') {
            line += 1;
            col = 0;
        }
        else {
            col += 1;
        }
        if (++i >= offset) {
            break;
        }
    }
    return Location {
        .file = shared_from_this(),
        .line = line,
        .column = col,
        .offset = offset,
    };
}
