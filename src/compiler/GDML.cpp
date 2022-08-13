#include "GDML.hpp"
#include <utils/Types.hpp>
#include <fstream>
#include "Instance.hpp"

using namespace gdml;
using namespace gdml::io;

#define GDML_LINE   "========================"
#define GDML_W_LINE "------------------------"

std::unordered_map<LanguageRule, bool> DEFAULT_RULES = {
    { LanguageRule::DefaultStaticFunctions, true },
};

GDML::GDML(Flags flags, IO& io)
 : m_flags(flags), m_io(io), m_rules(DEFAULT_RULES) {}

Error GDML::compileFile(SourceFile* src, SourceFile* dest) {
    return Instance(*this, src, dest).execute();
}

Error GDML::compileFile(std::string const& input, std::string const& output) {
    std::ifstream file(input);
    if (file.is_open()) {
        SourceFile src {
            input,
            std::string(
                (std::istreambuf_iterator<char>(file)),
                std::istreambuf_iterator<char>()
            )
        };
        SourceFile dest {
            output,
            std::string()
        };
        return compileFile(&src, &dest);
    }
    return Error::CantOpenFile;
}

bool GDML::getFlag(Flags::Value flag) const {
    return m_flags & flag;
}

void GDML::formatLines(LineError const& error) {
    size_t lineNum = error.start.line;
    auto linePadding = std::to_string(error.end.line + 1).size();

    for (auto& line : error.source->linesFrom(error.start, error.end)) {
        auto lineNumStr = std::to_string(lineNum + 1);

        // line number
        m_io
            // padding
            << std::string(linePadding - lineNumStr.size(), ' ')
            << Color::Lemon << lineNumStr
            << Color::Gray << " | "
            << Color::White << line << "\n";
        
        size_t squigglesStart;
        size_t squigglesCount;
        // first line
        if (lineNum == error.start.line) {
            // one line error?
            if (error.start.line == error.end.line) {
                // from start to end
                squigglesCount = error.end.column - error.start.column;
                squigglesStart = error.start.column;
            } else {
                // from start to end of line
                squigglesCount = line.size() - error.start.column;
                squigglesStart = error.start.column;
            }
        }
        // last line
        else if (lineNum == error.end.line) {
            squigglesCount = error.end.column;
            squigglesStart = 0;
        }
        // mid line
        else {
            squigglesCount = line.size();
            squigglesStart = 0;
        }

        // squiggles
        m_io
            << Color::Red
            << std::string(linePadding + 3 + squigglesStart, ' ')
            << std::string(squigglesCount, '~')
            << "\n";

        lineNum++;
    }
    
    // reset color
    m_io.color(Color::White);
}

void GDML::logError(LineError const& error) {
    if (m_flags ^ Flags::LogErrors) return;

    std::lock_guard lock(m_ioMutex);

    m_errorMutex.lock();
    m_encounteredErrors = true;
    m_errorMutex.unlock();

    m_io
        << Color::Gray << GDML_W_LINE "\n"
        << Color::Red
        << "Error " << error.code
            << " in " << Color::Lemon << error.source->path << Color::Red
            << " from " << error.start << " to " << error.end
            << Color::Red << ":\n";
    
    formatLines(error);

    m_io
        << Color::Red << error.message
        << Color::Cyan  << (error.hint.size() ? "\n(Hint: " + error.hint + ") " : "")
        << Color::Lemon << (error.note.size() ? "\n(Note: " + error.note + ") " : "")
        << "\n\n" << Color::White;
}

void GDML::logWarning(LineError const& error) {
    if (m_flags ^ Flags::LogWarnings) return;

    std::lock_guard lock(m_ioMutex);

    m_io
        << Color::Gray << GDML_W_LINE "\n"
        << Color::Yellow
        << "Warning " << error.code
            << " in " << Color::Lemon << error.source->path << Color::Red
            << " from " << error.start << " to " << error.end
            << Color::Yellow << ":\n";
    
    formatLines(error);

    m_io
        << Color::Yellow << error.message
        << Color::Cyan  << (error.hint.size() ? "\n(Hint: " + error.hint + ") " : "")
        << Color::Lemon << (error.note.size() ? "\n(Note: " + error.note + ") " : "")
        << "\n\n" << Color::White;
}

void GDML::logMessage(LineError const& error) {
    if (m_flags ^ Flags::LogMessages) return;

    std::lock_guard lock(m_ioMutex);

    m_io
        << Color::Gray << GDML_W_LINE "\n"
        << Color::Yellow
        << "Message " << error.code
            << " in " << Color::Lemon << error.source->path << Color::Red
            << " from " << error.start << " to " << error.end
            << Color::Yellow << ":\n";
    
    formatLines(error);

    m_io
        << Color::Yellow << error.message
        << Color::Cyan  << (error.hint.size() ? "\n(Hint: " + error.hint + ") " : "")
        << Color::Lemon << (error.note.size() ? "\n(Note: " + error.note + ") " : "")
        << "\n\n" << Color::White;
}

void GDML::logDebug(std::string const& message) {
    if (m_flags ^ Flags::LogDebug) return;
    std::lock_guard lock(m_ioMutex);
    m_io << "[DEBUG] " << message << "\n";
}

bool GDML::encounteredErrors() const {
    std::lock_guard lock(m_errorMutex);
    return m_encounteredErrors;
}

bool GDML::getRule(LanguageRule rule) const {
    if (!m_rules.count(rule)) return false;
    return m_rules.at(rule);
}

bool GDML::setRule(LanguageRule rule, bool value) {
    auto old = m_rules.at(rule);
    m_rules.at(rule) = value;
    return old;
}

std::mutex& GDML::rawIoLock() {
    return m_ioMutex;
}

IO& GDML::rawIO() {
    return m_io;
}
