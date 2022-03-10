#pragma once

#include <Geode.hpp>
#include "HotNodeManager.hpp"

USE_GEODE_NAMESPACE();

namespace gdml {
    Result<ccColor3B> GDML_DLL parseColor(std::string str);
    Result<CCRect> GDML_DLL parseRect(std::string const& str);
    std::string GDML_DLL rectToCppString(CCRect const& rect);
    std::string GDML_DLL ccColor3BToCppString(ccColor3B const& color);
    std::string GDML_DLL floatFormat(std::string f);
    std::string GDML_DLL floatFormat(float f);
}
