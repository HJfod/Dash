#include "GDML.hpp"

using namespace std::string_literals;

enum class LayoutAlign {
    Start,
    Middle,
    End,
};

LayoutAlign alignFromString(const char* str) {
    if (!str) return LayoutAlign::Middle;
    switch (hash(str)) {
        case hash("start"): return LayoutAlign::Start;
        case hash("middle"): return LayoutAlign::Middle;
        case hash("end"): return LayoutAlign::End;
        default: return LayoutAlign::Middle;
    }
}

float calcAlignFromString(const char* str, float width) {
    if (!str) return -width / 2;
    switch (hash(str)) {
        case hash("start"): return 0.f;
        case hash("middle"): return -width / 2;
        case hash("end"): return -width;
        default: return -width / 2;
    }
}

GDML* GDML::get() {
    static auto inst = new GDML();
    return inst;
}

void GDML::acquireVarName(tinyxml2::XMLElement* elem, CCNode* node) {
    if (m_varNames.count(node)) return;
    auto name = elem->Attribute("var");
    if (name) {
        m_varNames[node] = name;
    } else {
        m_varNames[node] = "var" + std::to_string(m_varNames.size());
    }
}

Result<CCNode*> GDML::createNode(tinyxml2::XMLElement* child, cocos2d::CCNode* node) {
    m_cppData << "\n";
    switch (hash(child->Name())) {
        case hash("menu"): case hash("CCMenu"): {
            auto menu = CCMenu::create();
            this->acquireVarName(child, menu);
            m_cppData << "auto " << m_varNames[menu] << " = CCMenu::create();\n";
            if (node) {
                node->addChild(menu);
                Managed::get()->add(menu);
            }
            this->handleEdit(child, menu);

            auto res = this->parseRecursive(child, menu);
            if (!res) return Err<>(res.error());

            if (node) {
                m_cppData << m_varNames[node] << "->addChild(" << m_varNames[menu] << ");\n";
            }
            m_cppData << "\n";

            return Ok<CCNode*>(menu);
        } break;

        case hash("node"): case hash("CCNode"): {
            auto cnode = CCNode::create();
            this->acquireVarName(child, cnode);
            m_cppData << "auto " << m_varNames[cnode] << " = CCNode::create();\n";
            if (node) {
                node->addChild(cnode);
                Managed::get()->add(cnode);
            }
            this->handleEdit(child, cnode);

            auto res = this->parseRecursive(child, cnode);
            if (!res) return Err<>(res.error());

            if (node) {
                m_cppData << m_varNames[node] << "->addChild(" << m_varNames[cnode] << ");\n";
            }
            m_cppData << "\n";

            return Ok<CCNode*>(cnode);
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
            this->acquireVarName(child, label);
            m_cppData << "auto " << m_varNames[label] << " = CCLabelBMFont::create(\""
                << text << "\", \"" << font << "\");\n";
            if (node) {
                node->addChild(label);
                Managed::get()->add(label);
            }
            this->handleEdit(child, label);

            auto res = this->parseRecursive(child, label);
            if (!res) return Err<>(res.error());
            if (node) {
                m_cppData << m_varNames[node] << "->addChild(" << m_varNames[label] << ");\n";
            }
            m_cppData << "\n";

            return Ok<CCNode*>(label);
        } break;

        case hash("sprite"): case hash("CCSprite"): {
            CCSprite* sprite = nullptr;
            auto src = child->Attribute("src");
            if (src) sprite = CCSprite::create(src);
            auto frame = child->Attribute("frame");
            if (frame) sprite = CCSprite::createWithSpriteFrameName(frame);
            if (sprite) {
                this->acquireVarName(child, sprite);
                m_cppData << "auto " << m_varNames[sprite] << " = CCSprite::" << 
                    (src ? "create" : "createWithSpriteFrameName") << "(\""
                    << (src ? src : frame) << "\");\n";
                if (node) {
                    node->addChild(sprite);
                    Managed::get()->add(sprite);
                }
                this->handleEdit(child, sprite);

                auto res = this->parseRecursive(child, sprite);
                if (!res) return Err<>(res.error());
                if (node) {
                    m_cppData << m_varNames[node] << "->addChild(" << m_varNames[sprite] << ");\n";
                }
                m_cppData << "\n";
                return Ok<CCNode*>(sprite);
            }
            return Err<>("Invalid source for sprite");
        } break;

        case hash("ssprite"): case hash("CCScale9Sprite"): {
            CCScale9Sprite* sprite = nullptr;
            auto arect = child->Attribute("rect");
            const char* src;
            const char* frame;
            CCRect rect;
            if (arect) {
                auto rrect = parseRect(arect);
                if (!rrect) return Err<>(rrect.error());
                rect = rrect.value();
                src = child->Attribute("src");
                if (src) sprite = CCScale9Sprite::create(src, rect);
                frame = child->Attribute("frame");
                if (frame) sprite = CCScale9Sprite::createWithSpriteFrameName(frame, rect);
            } else {
                src = child->Attribute("src");
                if (src) sprite = CCScale9Sprite::create(src);
                frame = child->Attribute("frame");
                if (frame) sprite = CCScale9Sprite::createWithSpriteFrameName(frame);
            }
            if (sprite) {
                this->acquireVarName(child, sprite);
                m_cppData << "auto " << m_varNames[sprite] << " = CCScale9Sprite::";
                if (arect) {
                    m_cppData << (src ? "create" : "createWithSpriteFrameName")
                        << "(\"" << (src ? src : frame) << "\", " << rectToCppString(rect) << ");\n";
                } else {
                    m_cppData << (src ? "create" : "createWithSpriteFrameName")
                        << "(\"" << (src ? src : frame) << "\")\n";
                }
                if (node) {
                    node->addChild(sprite);
                    Managed::get()->add(sprite);
                }
                this->handleEdit(child, sprite);

                auto res = this->parseRecursive(child, sprite);
                if (!res) return Err<>(res.error());
                if (node) {
                    m_cppData << m_varNames[node] << "->addChild(" << m_varNames[sprite] << ");\n";
                }
                m_cppData << "\n";
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
                this->acquireVarName(child, sprite);
                m_cppData << "auto " << m_varNames[sprite] << " = ButtonSprite::create(\""
                    << text << "\", \"" << font << "\", \"" << src << "\", " << floatFormat(scale) << ");\n";
                if (node) {
                    node->addChild(sprite);
                    Managed::get()->add(sprite);
                }
                this->handleEdit(child, sprite);

                auto res = this->parseRecursive(child, sprite);
                if (!res) return Err<>(res.error());
                if (node) {
                    m_cppData << m_varNames[node] << "->addChild(" << m_varNames[sprite] << ");\n";
                }
                m_cppData << "\n";
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
            auto callback = child->Attribute("onclick");
            if (btn) {
                this->acquireVarName(child, btn);
                m_cppData << "auto " << m_varNames[btn] << " = CCMenuItemSpriteExtra::create("
                    << m_varNames[sprNode.value()] << ", " << m_varNames[node] << ", " <<
                    (callback ? "menu_selector("s + callback + ")" : "nullptr") << ");\n";
                if (node) {
                    node->addChild(btn);
                    Managed::get()->add(btn);
                }
                this->handleEdit(child, btn);
                Managed::get()->add(btn);

                auto res = this->parseRecursive(child, btn);
                if (!res) return Err<>(res.error());
                if (node) {
                    m_cppData << m_varNames[node] << "->addChild(" << m_varNames[btn] << ");\n";
                }
                m_cppData << "\n";
                return Ok<CCNode*>(btn);
            }
            return Err<>("Invalid sprite for button");
        } break;

        default: return Err<>("Unknown node");
    }
    return Err<>("Unknown error");
}

Result<std::vector<CCNode*>> GDML::createLayout(tinyxml2::XMLElement* child, CCNode* parent) {
    m_cppData << "\n";
    switch (hash(child->Name())) {
        case hash("l-row"): {
            auto res = this->parseRecursive(child, parent);
            if (!res) return res;
            float pad = 0.f;
            auto paddingText = child->Attribute("pad");
            if (paddingText) {
                try { pad = std::stof(paddingText); } catch(...) {}
            }
            auto pos = this->handleLayoutEdit(child, parent);
            auto nodes = res.value();
            std::reverse(nodes.begin(), nodes.end());
            float width = 0;
            for (auto& node : nodes) {
                width += node->getScaledContentSize().width + pad;
            }
            width -= pad;
            float x = 0;
            float alignOffset = calcAlignFromString(child->Attribute("align"), width);
            for (auto& node : nodes) {
                x += node->getScaledContentSize().width / 2;
                auto aligned = x + alignOffset;
                node->setPositionX((pos.x == g_none ? 0.f : pos.x) + aligned);
                if (pos.x != g_none) {
                    m_cppData << m_varNames[node] << "->setPositionX("
                        << m_varNames[parent] + "_layoutX"
                        << (aligned > 0 ? " + " + floatFormat(aligned) : (aligned < 0 ? " - " + floatFormat(-aligned) : ""))
                        << ");\n";
                } else if (aligned) {
                    m_cppData << m_varNames[node] << "->setPositionX(" << floatFormat(aligned) << ");\n";
                }
                if (pos.y != g_none) {
                    auto y = node->getPositionY();
                    node->setPositionY(pos.y + y);
                    m_cppData << m_varNames[node] << "->setPositionY(" << m_varNames[parent] + "_layoutY"
                        << (y > 0 ? " + " + floatFormat(y) : (y < 0 ? " - " + floatFormat(-y) : "")) << ");\n";
                }
                x += node->getScaledContentSize().width / 2 + pad;
            }
            m_cppData << "\n";
            return Ok<std::vector<CCNode*>>({ res.value() });
        } break;

        case hash("l-column"): {
            auto res = this->parseRecursive(child, parent);
            if (!res) return res;
            float pad = 0.f;
            auto paddingText = child->Attribute("pad");
            if (paddingText) {
                try { pad = std::stof(paddingText); } catch(...) {}
            }
            auto pos = this->handleLayoutEdit(child, parent);
            auto nodes = res.value();
            float height = 0;
            for (auto& node : nodes) {
                height += node->getScaledContentSize().height + pad;
            }
            height -= pad;
            float y = 0;
            float alignOffset = calcAlignFromString(child->Attribute("align"), height);
            for (auto& node : nodes) {
                y += node->getScaledContentSize().height / 2;
                auto aligned = y + alignOffset;
                node->setPositionY((pos.y == g_none ? 0.f : pos.y) + aligned);
                if (pos.y != g_none) {
                    m_cppData << m_varNames[node] << "->setPositionY("
                        << m_varNames[parent] + "_layoutY"
                        << (aligned > 0 ? " + " + floatFormat(aligned) : (aligned < 0 ? " - " + floatFormat(-aligned) : ""))
                        << ");\n";
                } else if (aligned) {
                    m_cppData << m_varNames[node] << "->setPositionY(" << floatFormat(aligned) << ");\n";
                }
                if (pos.x != g_none) {
                    auto x = node->getPositionX();
                    node->setPositionX(pos.x + x);
                    m_cppData << m_varNames[node] << "->setPositionX(" << m_varNames[parent] + "_layoutX"
                        << (x > 0 ? " + " + floatFormat(x) : (x < 0 ? " - " + floatFormat(-x) : "")) << ");\n";
                }
                y += node->getScaledContentSize().height / 2 + pad;
            }
            m_cppData << "\n";
            return Ok<std::vector<CCNode*>>({ res.value() });
        } break;

        default: {
            auto res = this->createNode(child, parent);
            if (!res) return Err<>(res.error());
            return Ok<std::vector<CCNode*>>({ res.value() });
        } break;
    }
    return Err<>("Unknown error");
}

Result<std::vector<CCNode*>> GDML::parseRecursive(tinyxml2::XMLElement* elem, cocos2d::CCNode* node) {
    std::vector<CCNode*> nodes;
    for (auto child = elem->FirstChildElement(); child; child = child->NextSiblingElement()) {
        auto name = child->Name();
        bool wasIx = false;
        try {
            auto ix = std::stoi(name);
            wasIx = true;
            if (node->getChildrenCount() > static_cast<unsigned int>(ix)) {
                auto parent = node;
                node = as<CCNode*>(node->getChildren()->objectAtIndex(ix));
                this->acquireVarName(child, node);
                m_cppData << "auto " << m_varNames[node] << " = reinterpret_cast<CCNode*>("
                    << m_varNames[parent] << "->getChildren()->objectAtIndex(" << ix << "));\n";

                auto res = this->handleEdit(child, node);
                if (!res) return Err<>(res.error());

                auto pres = this->parseRecursive(child, node);
                if (!pres) return pres;

                nodes.push_back(node);
            } else {
                Log::get() << "Attempted to get child at " << ix << " out of bounds";
                continue;
            }
        } catch(...) {}
        
        if (!wasIx) {
            switch (hash(name)) {
                case hash("edit"): {
                    auto res = this->handleEdit(child, node);
                    if (!res) return Err<>(res.error());
                } break;

                default: { 
                    auto res = this->createLayout(child, node);
                    if (!res) return res;
                    vector_utils::push(nodes, res.value());
                } break;
            }
        }
    }
    return Ok<std::vector<CCNode*>>(nodes);
}

Result<> GDML::parse(tinyxml2::XMLDocument* doc) {
    Log::get() << "Refreshing";
    
    Managed::get()->clear();
    m_varNames.clear();

    auto gdml = doc->FirstChildElement("gdml");
    if (!gdml) {
        return Err<>("Missing <gdml> tag");
    }
    auto scene = CCDirector::sharedDirector()->getRunningScene();
    this->acquireVarName(gdml, scene);
    m_cppData << "auto " << m_varNames[scene] << " = CCDirector::sharedDirector()->getRunningScene();\n\n";
    auto res = this->parseRecursive(gdml, scene);
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
    auto p = this->parse(&doc);
    if (!p) return p;

    auto cppPath = path;
    cppPath.replace_extension("gdml.cpp");
    auto cppData = m_cppData.str();
    while (string_utils::contains(cppData, "\n\n\n")) {
        string_utils::replaceIP(cppData, "\n\n\n", "\n\n");
    }
    m_cppData.str("");
    auto write = file_utils::writeString(cppPath, cppData);
    if (!write) {
        return Err<>(write.error());
    }
    return Ok<>();
}
