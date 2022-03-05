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

std::string rectToCppString(CCRect const& rect) {
    return "CCRect { " +
        std::to_string(rect.origin.x) + ", " +
        std::to_string(rect.origin.y) + ", " +
        std::to_string(rect.size.width) + ", " +
        std::to_string(rect.size.height) + " }";
}

std::string ccColor3BToCppString(ccColor3B const& color) {
    return "{ " +
        std::to_string(color.r) + ", " +
        std::to_string(color.g) + ", " +
        std::to_string(color.b) + " }";
}

std::string floatFormat(std::string f) {
    if (string_utils::contains(f, ".")) {
        while (f._Starts_with("0")) f = f.substr(1);
        while (string_utils::endsWith(f, "0")) f.pop_back();
        return f + "f";
    }
    return f + ".f";
}

std::string floatFormat(float f) {
    return floatFormat(std::to_string(f));
}
