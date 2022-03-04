#include "../mod/src/parse/Managed.hpp"

HOT_UI_BUILD(CCNode* target) {
    auto winSize = CCDirector::sharedDirector()->getWinSize();

    auto sprite = M<CCSprite>::create("GJ_button_01.png");
    sprite->setPosition(winSize / 2);
    sprite->setScale(5.f);
    sprite->setScaleY(1.f);
    target->addChild(sprite);

    auto label = M<CCLabelBMFont>::create("Holy hell", "bigFont.fnt");
    label->setPosition(winSize / 2);
    target->addChild(label);
}

