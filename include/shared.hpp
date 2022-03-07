#pragma once

#include <Geode.hpp>
#include "HotNodeManager.hpp"

USE_GEODE_NAMESPACE();

namespace gdml {
    Result<ccColor3B> parseColor(std::string str);
    Result<CCRect> parseRect(std::string const& str);
    std::string rectToCppString(CCRect const& rect);
    std::string ccColor3BToCppString(ccColor3B const& color);
    std::string floatFormat(std::string f);
    std::string floatFormat(float f);
}
