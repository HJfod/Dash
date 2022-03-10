#include <Geode.hpp>
#include "utils/FileWatcher.hpp"
#include <GDML.hpp>

USE_GEODE_NAMESPACE();
using namespace std::chrono_literals;

GEODE_API bool GEODE_CALL geode_load(Mod* mod) {
    return true;
}

GEODE_API void GEODE_CALL geode_unload() {
}
