#include <Geode.hpp>

USE_GEODE_NAMESPACE();

GEODE_API bool GEODE_CALL geode_load(Mod* mod) {
    mod->log() << "hi" << geode::endl;

    return true;
}
