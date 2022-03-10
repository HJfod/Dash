#include "TestLayer.hpp"
#include "GDML.hpp"

using namespace gdml;

class $modify(MenuLayer) {
    void onMoreGames(CCObject*) {
        R"(
            <goto>
                <scene>
                    <TestLayer />
                </scene>
            </goto>
        )"_gdml;
    }
};
