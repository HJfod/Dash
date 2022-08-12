#include "MyLayer.hpp"

class $modify(MenuLayer) {
    bool init() {
        if (!MenuLayer::init())
            return false;
        
        return true;
    }
};
