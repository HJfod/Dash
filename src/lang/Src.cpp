#include <lang/Main.hpp>
#include <Geode/utils/file.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

bool Location::operator==(Location const& other) const {
    return src == other.src && line == other.line && column == other.column;
}

std::string Location::toString() const {
    return fmt::format("{}:{}", line, column);
}

std::string Range::toString() const {
    if (start == end) {
        return start.toString();
    }
    return fmt::format("{}-{}", start.toString(), end.toString());
}

std::string Message::toString() const {
    std::string res = "";
    switch (this->level) {
        case Level::Info: res += "Note"; break;
        case Level::Error: res += "Error"; break;
        case Level::Warning: res += "Warning"; break;
        default: res += "Unknown"; break;
    }
    res += " at " + this->range.toString() + " in " + this->src->getName() + ":\n";
    res += this->info;
    return res;
}

std::string SrcFile::getName() const {
    return m_path.generic_string();
}

SrcFile::SrcFile(ghc::filesystem::path const& path, std::string const& data)
  : m_path(path), m_data(data) {} 

Result<Rc<SrcFile>> SrcFile::from(ghc::filesystem::path const& path) {
    GEODE_UNWRAP_INTO(auto data, file::readString(path));
    return Ok(std::make_shared<SrcFile>(path, data));
}

Stream::Stream(Rc<Src> file) : m_file(file) {}

char Stream::peek() const {
    if (m_file->size() < m_position) {
        return m_file->at(m_position);
    }
    return '\0';
}

std::string Stream::peek(size_t count) const {
    if (m_file->size() < m_position + count) {
        return m_file->from(m_position, count);
    }
    return "";
}

char Stream::next() {
    if (m_file->size() < m_position) {
        return m_file->at(m_position++);
    }
    return '\0';
}

bool Stream::eof() const {
    return m_file->size() >= m_position;
}

void Stream::navigate(size_t offset) {
    m_position = offset;
}

Rc<Src> Stream::src() const {
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

std::vector<Message> Stream::errors() const {
    std::vector<Message> errs;
    for (auto& msg : m_messages) {
        if (msg.level == Level::Error) {
            errs.push_back(msg);
        }
    }
    return errs;
}

SrcFile::~SrcFile() {
    // mm i sure hope no one makes two SrcFiles to the same file!
    file::unwatchFile(m_path);
}

Stream SrcFile::read() {
    return Stream(shared_from_this());
}

bool SrcFile::onChanged(std::function<void(Rc<Src>)> callback) {
    if (!file::watchFile(m_path)) {
        return false;
    }
    m_listener = std::make_unique<EventListener<file::FileWatchFilter>>(
        [=](file::FileWatchEvent*) {
            callback(shared_from_this());
        },
        file::FileWatchFilter(m_path)
    );
    return true;
}

size_t SrcFile::size() const {
    return m_data.size();
}

char SrcFile::at(size_t offset) const {
    return m_data[offset];
}

std::string SrcFile::from(size_t offset, size_t count) const {
    return m_data.substr(offset, count);
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
        .src = shared_from_this(),
        .line = line,
        .column = col,
        .offset = offset,
    };
}
