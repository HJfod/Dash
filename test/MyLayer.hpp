#pragma once

#include <Geode/Geode.hpp>

USE_GEODE_NAMESPACE();

class MyLayer : public CCLayer {
protected:
    bool init(gd::string const& title);

public:
    static MyLayer* create(gd::string const& title);
};
