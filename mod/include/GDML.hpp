#pragma once

#include <Geode/DefaultInclude.hpp>

#ifdef GEODE_IS_WINDOWS
    #ifdef HJFOD_GDML_EXPORTING
        #define GDML_DLL __declspec(dllexport)
    #else
        #define GDML_DLL __declspec(dllimport)
    #endif
#else
    #define GDML_DLL
#endif

namespace gdml {
    GDML_DLL void loadGDMLFromFile(cocos2d::CCNode* node, ghc::filesystem::path const& path);
}
