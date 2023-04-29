#include <Geode/modify/MenuLayer.hpp>
#include <GDML.hpp>

using namespace geode::prelude;

struct $modify(MenuLayer) {
    bool init() {
        if (!MenuLayer::init())
            return false;

        gdml::loadGDMLFromFile(this, Mod::get()->getResourcesDir() / "MenuLayer.gdml");

        return true;
    }
};
