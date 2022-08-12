#include "MyLayer.hpp"

// MyLayer::init defined in MyLayer.gdml

MyLayer* MyLayer::create(gd::string const& title) {
    auto ret = new MyLayer();
    if (ret && ret->init(title)) {
        ret->autorelease();
        return ret;
    }
    CC_SAFE_DELETE(ret);
    return nullptr;
}

