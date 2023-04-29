#pragma once

#include "lang/Parser.hpp"

namespace gdml {
    class State : public cocos2d::CCNode {
    public:
        static State* create(ghc::filesystem::path const& file);
    };
}
