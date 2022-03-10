#include "TestLayer.hpp"

bool TestLayer::init() {
    if (!CCNode::init())
        return false;

    auto layer = GDML::get()->parseFile("K:/GeodeSDK/HJ/GDML/test/test.gdml");
    if (layer) {
        this->addChild(layer.value().at(0));
    }
    return true;
}

TestLayer* TestLayer::create() {
    auto ret = new TestLayer;
    if (ret && ret->init()) {
        ret->autorelease();
        return ret;
    }
    CC_SAFE_DELETE(ret);
    return nullptr;
}


