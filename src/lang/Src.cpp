#include <lang/Src.hpp>
#include <lang/Token.hpp>
#include <lang/State.hpp>
#include <Geode/utils/file.hpp>

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

bool Location::operator==(Location const& other) const {
    return src == other.src && line == other.line && column == other.column;
}

std::string Location::toString() const {
    return fmt::format("{}:{}", line + 1, column + 1);
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
    if (this->src) {
        res += " at " + this->range.toString() + " in " + this->src->getName() + ":\n";
        res += this->src->getUnderlined(this->range) + "\n";
    }
    res += this->info;
    return res;
}

std::string SrcFile::getName() const {
    return m_path.generic_string();
}

SrcFile::SrcFile(Path const& path, std::string const& data)
  : m_path(path), m_data(data) {} 

Result<Rc<SrcFile>> SrcFile::from(Path const& path) {
    GEODE_UNWRAP_INTO(auto data, file::readString(path));
    return Ok(std::make_shared<SrcFile>(path, data));
}

Stream::Stream(Rc<Src> file, UnitParser& state) : m_file(file), m_state(state) {}

Stream::~Stream() = default;

char Stream::peek() const {
    if (m_position < m_file->size()) {
        return m_file->at(m_position);
    }
    return '\0';
}

std::string Stream::peek(size_t count) const {
    if (m_position + count < m_file->size()) {
        return m_file->from(m_position, count);
    }
    return "";
}

char Stream::next() {
    if (m_position < m_file->size()) {
        return m_file->at(m_position++);
    }
    return '\0';
}

bool Stream::eof() const {
    return m_position >= m_file->size();
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

void Stream::debugTick(std::source_location const loc) {
    if (m_debugTickCounter++ > 10000000) {
        throw std::runtime_error(fmt::format(
            "Debug tick level exceeded 10000000 limit. "
            "This implies that the compiler ended up in an infinite loop - "
            "please report this bug! (Occurred in {} at {}:{} in {})",
            loc.file_name(), loc.line(), loc.column(), loc.function_name()
        ));
    }
}

void Stream::setLastToken(Token const& token) {
    m_lastToken = std::make_unique<Token>(token);
}

Option<Token> Stream::last() const {
    if (m_lastToken) {
        return *m_lastToken;
    }
    return None;
}

UnitParser& Stream::state() const {
    return m_state;
}

SrcFile::~SrcFile() {
    // mm i sure hope no one makes two SrcFiles to the same file!
    file::unwatchFile(m_path);
}

Stream SrcFile::read(UnitParser& state) {
    return Stream(shared_from_this(), state);
}

std::string SrcFile::getUnderlined(Range const& range) const {
    std::vector<std::string> lines { "" };
    size_t line = 0;
    for (auto& c : m_data) {
        if (c == '\n') {
            if (line >= range.start.line && line < range.end.line) {
                lines.push_back("");
            }
            line += 1;
        }
        else if (line >= range.start.line && line <= range.end.line) {
            if (c != '\r') {
                lines.back() += c;
            }
        }
    }
    std::string res;
    line = 0;
    for (auto& data : lines) {
        if (line > 0) {
            res += "\n";
        }
        res += data + "\n";
        size_t len;
        if (line == 0) {
            res += std::string(range.start.column, ' ');
            if (lines.size() == 1) {
                len = range.end.column - range.start.column;
            }
            else {
                len = data.size() - range.start.column;
            }
        }
        else if (line == lines.size() - 1) {
            len = range.end.column;
        }
        else {
            len = data.size();
        }
        if (len == 0) {
            len = 1;
        }
        res += std::string(len, '~');
        line += 1;
    }
    return res;
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

Path SrcFile::getSearchDir() const {
    return m_path.parent_path();
}

Path SrcFile::getPath() const {
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

Rollback::Rollback(Stream& stream, std::source_location const loc)
  : m_stream(stream),
    m_offset(stream.offset()),
    m_msgLevel(stream.state().getShared().pushLogLevel())
{
    stream.debugTick(loc);
    if (auto last = stream.last()) {
        m_lastToken = std::make_unique<Token>(last.value());
    }
}

Rollback::~Rollback() {
    if (!m_commit) {
        m_stream.navigate(m_offset);
        if (m_lastToken) {
            m_stream.setLastToken(*m_lastToken);
        }
        m_commit = true;
    }
    m_stream.state().getShared().popLogLevel();
}

impl::Failure<size_t> Rollback::error(Message const& msg) {
    m_stream.state().getShared().log(msg, m_msgLevel);
    return Err(m_stream.state().getShared().getErrors().size());
}

void Rollback::clearMessages() {
    // Remove all messages that have been pushed that have a higher or equal msg level
    // This is because rb.error just pushes errors to the stream's message 
    // queue, but sometimes those errors end up being part of a parse tree that's 
    // an optional branch and aren't actually errors but just signal that that 
    // branch isn't valid (like how Expr::pullPrimaryNonCall tries a bunch of 
    // different options)
    m_stream.state().getShared().popMessages(m_msgLevel);
}

void Rollback::commit() {
    m_commit = true;
    this->clearMessages();
}
