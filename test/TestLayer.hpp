#pragma once

#include <GDML.hpp>

using namespace gdml;

class TestLayer : public CCLayer {
protected:
    bool init();

public:
    static TestLayer* create();
};
static GDML_REGISTER(TestLayer);
