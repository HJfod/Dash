#pragma once

#include <Geode/DefaultInclude.hpp>

#ifdef GEODE_IS_WINDOWS
    #ifdef HJFOD_Dash_EXPORTING
        #define Dash_DLL __declspec(dllexport)
    #else
        #define Dash_DLL __declspec(dllimport)
    #endif
#else
    #define Dash_DLL
#endif

namespace dash {
    Dash_DLL void loadDashFromFile(cocos2d::CCNode* node, ghc::filesystem::path const& path);
}
