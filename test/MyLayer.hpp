#pragma once

#include <Geode/Geode.hpp>

USE_GEODE_NAMESPACE();

class MyLayer : public CCLayer {
protected:
    bool init(std::string const& title);

public:
    static MyLayer* create(std::string const& title);
};
