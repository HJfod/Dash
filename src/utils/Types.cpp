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

types::NumberType types::numberTypeFromString(std::string const& str) {
    switch (hash(str.c_str())) {
        default:          return NumberType::I32;
        case hash("i8"):  return NumberType::I8;
        case hash("i16"): return NumberType::I16;
        case hash("i32"): return NumberType::I32;
        case hash("i64"): return NumberType::I64;
        case hash("u8"):  return NumberType::U8;
        case hash("u16"): return NumberType::U16;
        case hash("u32"): return NumberType::U32;
        case hash("u64"): return NumberType::U64;
        case hash("f32"): return NumberType::F32;
        case hash("f64"): return NumberType::F64;
    }
}

std::string types::numberTypeToString(types::NumberType type) {
    switch (type) {
        default:              return "i32";
        case NumberType::I8:  return "i8";
        case NumberType::I16: return "i16";
        case NumberType::I32: return "i32";
        case NumberType::I64: return "i64";
        case NumberType::U8:  return "u64";
        case NumberType::U16: return "u64";
        case NumberType::U32: return "u64";
        case NumberType::U64: return "u64";
        case NumberType::F32: return "f32";
        case NumberType::F64: return "f64";
    }
}

std::string types::numberTypeToCppType(types::NumberType type) {
    switch (type) {
        default:              return "int";
        case NumberType::I8:  return "char";
        case NumberType::I16: return "short";
        case NumberType::I32: return "int";
        case NumberType::I64: return "long long";
        case NumberType::U8:  return "unsigned char";
        case NumberType::U16: return "unsigned short";
        case NumberType::U32: return "unsigned int";
        case NumberType::U64: return "unsigned long long";
        case NumberType::F32: return "float";
        case NumberType::F64: return "double";
    }
}

bool types::numberTypeIsUnsigned(types::NumberType type) {
    return
        type == NumberType::U8  ||
        type == NumberType::U16 ||
        type == NumberType::U32 ||
        type == NumberType::U64;
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
