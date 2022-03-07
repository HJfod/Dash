#pragma once

#include <Geode.hpp>

USE_GEODE_NAMESPACE();

#ifdef GDML_EXPORT
    #define GDML_DLL __declspec(dllexport)
#else
    #define GDML_DLL __declspec(dllimport)
#endif
