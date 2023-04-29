#include <Geode/modify/MenuLayer.hpp>
#include <GDML.hpp>

struct $modify(MenuLayer) {
    bool init() {
        if (!MenuLayer::init())
            return false;

        auto state = gdml::State::create(Mod::get()->getResourcesDir() / "MenuLayer.gdml");
        this->addChild(state);

        return true;
    }
};
