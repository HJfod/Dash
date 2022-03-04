#include "shared.hpp"

Result<ccColor3B> parseColor(std::string str) {
    if (string_utils::contains(str, ",")) {
        auto colors = string_utils::split(str, ",");
        if (colors.size() == 3) {
            try {
                auto r = std::stoi(colors[0]);
                auto g = std::stoi(colors[1]);
                auto b = std::stoi(colors[2]);
                if (
                    0 <= r && r <= 255 &&
                    0 <= g && g <= 255 &&
                    0 <= b && b <= 255
                ) {
                    return Ok<ccColor3B>({
                        static_cast<GLubyte>(r),
                        static_cast<GLubyte>(g),
                        static_cast<GLubyte>(b),
                    });
                } else {
                    return Err<>("Color is an RGB string whose elements are not all within [0, 255]");
                }
            } catch(...) {
                return Err<>("Color is an RGB string whose elements are not all ints");
            }
        } else {
            return Err<>("Color is a string that is not in the format \"r,g,b\" or a hex number");
        }
    } else {
        if (str[0] == '#') {
            str = str.substr(1);
        }
        if (str.size() > 6) {
            return Err<>("Color is too long for a hex string");
        }
        try {
            auto color = std::stoi(str, nullptr, 16);
            return Ok<>(cc3x(color));
        } catch(...) {
            return Err<>("Color is not a valid hex string");
        }
    }
}

Result<CCRect> parseRect(std::string const& str) {
    auto r = string_utils::split(str, ",");
    if (r.size() != 4) return Err<>("Rect does not appear to be valid");
    try {
        return Ok<CCRect>({
            std::stof(r.at(0)),
            std::stof(r.at(1)),
            std::stof(r.at(2)),
            std::stof(r.at(3)),
        });
    } catch(...) {
        return Err<>("Unable to parse correctly");
    }
}

