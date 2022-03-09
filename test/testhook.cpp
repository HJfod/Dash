#include "TestLayer.hpp"
#include "GDML.hpp"

using namespace gdml;

class $modify(MenuLayer) {
    void onMoreGames(CCObject*) {
        R"(
            <scene>
                <TestLayer />
            </scene>
        )"_gdml;
    }
};
