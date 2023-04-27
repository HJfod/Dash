#include "Main.hpp"
#include <Geode/utils/file.hpp>

SrcFile::SrcFile(ghc::filesystem::path const& path, std::string const& data)
  : m_path(path), m_data(data) {} 

Result<Rc<SrcFile>> SrcFile::from(ghc::filesystem::path const& path) {
    GEODE_UNWRAP_INTO(auto data, file::readString(path));
    return Ok(std::make_shared<SrcFile>(path, data));
}

Stream::Stream(Rc<SrcFile> file) : m_file(file) {}

char Stream::next() {
    if (m_file->getData().size() < m_position) {
        return m_file->getData()[m_position++];
    }
    return '\0';
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
