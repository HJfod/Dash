#include "GDML.hpp"
#include "../utils/exprtk.hpp"

GDML* GDML::get() {
    static auto inst = new GDML();
    return inst;
}

Result<> GDML::handleEdit(tinyxml2::XMLElement* elem, cocos2d::CCNode* node) {
    auto winSize = CCDirector::sharedDirector()->getWinSize();

    Managed::get()->touch(node);

    auto rgba = dynamic_cast<CCRGBAProtocol*>(node);
    auto label = dynamic_cast<CCLabelProtocol*>(node);

    exprtk::symbol_table<float> table;
    table.add_constant("x", node->getPositionX());
    table.add_constant("y", node->getPositionY());
    table.add_constant("w", node->getContentSize().width);
    table.add_constant("h", node->getContentSize().height);
    table.add_constant("sw", node->getScaledContentSize().width);
    table.add_constant("sh", node->getScaledContentSize().height);
    table.add_constant("ww", winSize.width);
    table.add_constant("wh", winSize.height);
    if (node->getParent()) {
        table.add_constant("px", node->getParent()->getPositionX());
        table.add_constant("py", node->getParent()->getPositionY());
        table.add_constant("pw", node->getParent()->getContentSize().width);
        table.add_constant("ph", node->getParent()->getContentSize().height);
    }

    exprtk::expression<float> exp;
    exp.register_symbol_table(table);

    exprtk::parser<float> parser;

    auto xEdit = elem->Attribute("x");
    if (xEdit) {
        auto val = std::string(xEdit);
        if (parser.compile(val, exp)) {
            node->setPositionX(exp.value());
        } else {
            Log::get() << "Error parsing math for X pos";
        }
    }

    auto yEdit = elem->Attribute("y");
    if (yEdit) {
        auto val = std::string(yEdit);
        if (parser.compile(val, exp)) {
            node->setPositionY(exp.value());
        } else {
            Log::get() << "Error parsing math for Y pos";
        }
    }

    auto scaleEdit = elem->Attribute("scale");
    if (scaleEdit) {
        try {
            auto val = std::string(scaleEdit);
            if (val._Starts_with(".")) val = "0" + val;
            node->setScale(std::stof(val));
        } catch(...) {}
    }

    auto rotateEdit = elem->Attribute("rotate");
    if (rotateEdit) {
        try {
            node->setRotation(std::stof(rotateEdit));
        } catch(...) {}
    }

    auto sizeEdit = elem->Attribute("size");
    if (sizeEdit) {
        auto val = std::string(sizeEdit);
        try {
            if (string_utils::contains(val, ':')) {
                auto width = std::stof(val.substr(0, val.find(':')));
                auto height = std::stof(val.substr(val.find(':') + 1));
                node->setContentSize({ width, height });
            }
        } catch(...) {}
    }

    auto colorEdit = elem->Attribute("color");
    if (colorEdit) {
        try {
            if (rgba) {
                auto color = parseColor(colorEdit);
                if (color) {
                    rgba->setColor(color.value());
                } else {
                    Log::get() << "Error parsing color: " << color.error();
                }
            } else {
                Log::get() << "color called on non-colorable node";
            }
        } catch(...) {}
    }

    auto opacityEdit = elem->Attribute("opacity");
    if (opacityEdit) {
        try {
            if (rgba) {
                rgba->setOpacity(static_cast<GLubyte>(std::stoi(opacityEdit)));
            } else {
                Log::get() << "opacity called on non-colorable node";
            }
        } catch(...) {}
    }

    auto textEdit = elem->Attribute("text");
    if (textEdit) {
        try {
            if (label) {
                label->setString(textEdit);
            } else {
                Log::get() << "text called on non-textable node";
            }
        } catch(...) {}
    }

    return Ok<>();
}

Result<CCNode*> GDML::createNode(tinyxml2::XMLElement* child, cocos2d::CCNode* node) {
    switch (hash(child->Name())) {
        case hash("menu"): case hash("CCMenu"): {
            auto menu = CCMenu::create();
            if (node) {
                node->addChild(menu);
                Managed::get()->add(menu);
            }
            this->handleEdit(child, menu);

            auto res = this->parseRecursive(child, menu);
            if (!res) return Err<>(res.error());

            return Ok<CCNode*>(menu);
        } break;

        case hash("label"): case hash("CCLabelBMFont"): {
            auto font = child->Attribute("font");
            if (!font) font = "bigFont.fnt";
            auto etext = child->FirstChild();
            std::string text = "";
            if (etext && (etext = etext->ToText())) {
                text = string_utils::trim(etext->Value());
            }

            auto label = CCLabelBMFont::create(text.c_str(), font);
            if (node) {
                node->addChild(label);
                Managed::get()->add(label);
            }
            this->handleEdit(child, label);

            auto res = this->parseRecursive(child, label);
            if (!res) return Err<>(res.error());

            return Ok<CCNode*>(label);
        } break;

        case hash("sprite"): case hash("CCSprite"): {
            CCSprite* sprite = nullptr;
            auto src = child->Attribute("src");
            if (src) sprite = CCSprite::create(src);
            auto frame = child->Attribute("frame");
            if (frame) sprite = CCSprite::createWithSpriteFrameName(frame);
            if (sprite) {
                if (node) {
                    node->addChild(sprite);
                    Managed::get()->add(sprite);
                }
                this->handleEdit(child, sprite);

                auto res = this->parseRecursive(child, sprite);
                if (!res) return Err<>(res.error());
                return Ok<CCNode*>(sprite);
            }
            return Err<>("Invalid source for sprite");
        } break;

        case hash("ssprite"): case hash("CCScale9Sprite"): {
            CCScale9Sprite* sprite = nullptr;
            auto arect = child->Attribute("rect");
            if (arect) {
                auto rect = parseRect(arect);
                if (!rect) return Err<>(rect.error());
                auto src = child->Attribute("src");
                if (src) sprite = CCScale9Sprite::create(src, rect.value());
                auto frame = child->Attribute("frame");
                if (frame) sprite = CCScale9Sprite::createWithSpriteFrameName(frame, rect.value());
            } else {
                auto src = child->Attribute("src");
                if (src) sprite = CCScale9Sprite::create(src);
                auto frame = child->Attribute("frame");
                if (frame) sprite = CCScale9Sprite::createWithSpriteFrameName(frame);
            }
            if (sprite) {
                if (node) {
                    node->addChild(sprite);
                    Managed::get()->add(sprite);
                }
                this->handleEdit(child, sprite);

                auto res = this->parseRecursive(child, sprite);
                if (!res) return Err<>(res.error());
                return Ok<CCNode*>(sprite);
            }
            return Err<>("Invalid source for sprite");
        } break;

        case hash("bsprite"): case hash("ButtonSprite"): {
            CCSprite* sprite = nullptr;
            auto src = child->Attribute("src");
            if (!src) src = "GJ_button_01.png";
            auto font = child->Attribute("font");
            if (!font) font = "bigFont.fnt";
            auto etext = child->FirstChild();
            auto tscale = child->Attribute("tscale");
            float scale = 1.f;
            if (tscale) {
                try { scale = std::stof(tscale); } catch(...) {}
            }
            std::string text = "";
            if (etext && (etext = etext->ToText())) {
                text = string_utils::trim(etext->Value());
            }
            if (src) sprite = ButtonSprite::create(
                text.c_str(), font, src, scale
            );
            if (sprite) {
                if (node) {
                    node->addChild(sprite);
                    Managed::get()->add(sprite);
                }
                this->handleEdit(child, sprite);

                auto res = this->parseRecursive(child, sprite);
                if (!res) return Err<>(res.error());
                return Ok<CCNode*>(sprite);
            }
            return Err<>("Invalid source for sprite");
        } break;

        case hash("button"): case hash("CCMenuItemSpriteExtra"): {
            auto spr = child->FirstChildElement();
            if (!spr) return Err<>("Button missing sprite");
            auto sprNode = this->createNode(spr, nullptr);
            if (!sprNode) return sprNode;
            child->DeleteChild(spr);
            auto btn = CCMenuItemSpriteExtra::create(
                sprNode.value(), node, nullptr
            );
            if (btn) {
                if (node) {
                    node->addChild(btn);
                    Managed::get()->add(btn);
                }
                this->handleEdit(child, btn);
                Managed::get()->add(btn);

                auto res = this->parseRecursive(child, btn);
                if (!res) return Err<>(res.error());
                return Ok<CCNode*>(btn);
            }
            return Err<>("Invalid sprite for button");
        } break;

        default: return Err<>("Unknown node");
    }
    return Err<>("Unknown error");
}

Result<> GDML::parseRecursive(tinyxml2::XMLElement* elem, cocos2d::CCNode* node) {
    for (auto child = elem->FirstChildElement(); child; child = child->NextSiblingElement()) {
        auto name = child->Name();
        try {
            auto ix = std::stoi(name);
            if (node->getChildrenCount() > static_cast<unsigned int>(ix)) {
                node = as<CCNode*>(node->getChildren()->objectAtIndex(ix));
                auto res = this->handleEdit(child, node);
                if (!res) return res;
                auto pres = this->parseRecursive(child, node);
                if (!pres) return pres;
                return Ok<>();
            } else {
                Log::get() << "Attempted to get child at " << ix << " out of bounds";
                continue;
            }
        } catch(...) {}

        switch (hash(name)) {
            case hash("edit"): {
                auto res = this->handleEdit(child, node);
                if (!res) return res;
            } break;

            case hash("macro"): {
                auto macroName = elem->Attribute("name");
                if (!macroName) {
                    
                }
            } break;

            default: return this->createNode(child, node);
        }
    }
    return Ok<>();
}

Result<> GDML::parse(tinyxml2::XMLDocument* doc) {
    Log::get() << "Refreshing";
    
    Managed::get()->clear();

    auto gdml = doc->FirstChildElement("gdml");
    if (!gdml) {
        return Err<>("Missing <gdml> tag");
    }
    auto res = this->parseRecursive(
        gdml, CCDirector::sharedDirector()->getRunningScene()
    );
    if (!res) {
        Log::get() << "Error generating: " << res.error();
        Managed::get()->clear();
    }
    return res;
}

Result<> GDML::parse(ghc::filesystem::path const& path) {
    auto read = file_utils::readString(path);
    if (!read) {
        return Err<>(read.error());
    }
    auto data = read.value();
    if (!data.size()) return Ok<>();

    tinyxml2::XMLDocument doc;
    auto r = doc.Parse(data.c_str(), data.size());
    if (r != tinyxml2::XML_SUCCESS) {
        return Err<>("Parse error " + std::to_string(r));
    }
    return this->parse(&doc);
}
