#include "Types.hpp"
#include "IO.hpp"
#include <sstream>

using namespace gdml;
using namespace gdml::io;

std::ostream& gdml::operator<<(std::ostream& stream, Error error) {
    return stream << static_cast<int>(error) << " (" << errorToString(error) << ")";
}

std::ostream& gdml::operator<<(std::ostream& stream, Position const& pos) {
    return stream << pos.toString();
}

types::DataType types::dataTypeFromString(std::string const& str) {
    size_t i = 0;
    for (auto& dt : DATATYPE_STRS) {
        if (dt == str) {
            return DATATYPES[i];
        }
        i++;
    }
    return DataType::Void;
}

std::string types::dataTypeToString(types::DataType type) {
    auto typeIndex = static_cast<size_t>(type);
    if (typeIndex >= 0 && typeIndex < DATATYPE_COUNT) {
        return DATATYPE_STRS[typeIndex];
    }
    return "void";
}

std::string types::dataTypeToCppType(types::DataType type) {
    auto typeIndex = static_cast<size_t>(type);
    if (typeIndex >= 0 && typeIndex < DATATYPE_COUNT) {
        return DATATYPE_CPP[typeIndex];
    }
    return "void";
}

bool types::dataTypeIsUnsigned(types::DataType type) {
    return
        type == DataType::U8  ||
        type == DataType::U16 ||
        type == DataType::U32 ||
        type == DataType::U64;
}

std::vector<std::string> SourceFile::linesFrom(
    Position const& start,
    Position const& end
) const {
    std::vector<std::string> lines {};

    std::istringstream iss(this->data);
    std::string line;
    size_t ix = 0;
    while (std::getline(iss, line)) {
        if (start.line <= ix && ix <= end.line) {
            lines.push_back(line);
        }
        ix++;
    }

    return std::move(lines);
}

NamespaceParts gdml::splitNamespaceString(std::string const& str) {
    std::vector<std::string> res;
    if (str.size()) {
        auto s = str;
        size_t pos = 0;
        while ((pos = s.find("::")) != std::string::npos) {
            res.push_back(s.substr(0, pos));
            s.erase(0, pos + 2);
        }
        res.push_back(s);
    }
    return res;
}
