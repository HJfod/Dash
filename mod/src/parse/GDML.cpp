#include "GDML.hpp"
#include <fmt/include/fmt/format.h>

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

std::string GDML::replaceVariables(const char* str) {
    return this->replaceVariables(str ? std::string(str) : ""s);
}

std::string GDML::replaceVariables(std::string const& str) {
    auto s = str;
    size_t ix = 0;
    for (auto& [varName, val] : m_variables) {
        // absolutely flabber-gastedly shitty fix for 
        // double-escaping
        ix++;
        if (ix < m_variables.size()) {
            string_utils::replaceIP(s, "{{", "__ESCAPED0__");
            string_utils::replaceIP(s, "}}", "__ESCAPED1__");
        }
        try {
            switch (val.m_type) {
                case Variable::Int:
                    s = fmt::format(s, fmt::arg(varName.c_str(), std::any_cast<int>(val.m_value)));
                    break;

                case Variable::Float:
                    s = fmt::format(s, fmt::arg(varName.c_str(), std::any_cast<float>(val.m_value)));
                    break;

                case Variable::String:
                    s = fmt::format(s, fmt::arg(varName.c_str(), std::any_cast<std::string>(val.m_value)));
                    break;
            }
        } catch(...) {}
        if (ix < m_variables.size()) {
            string_utils::replaceIP(s, "__ESCAPED0__", "{{");
            string_utils::replaceIP(s, "__ESCAPED1__", "}}");
        }
    }
    return s;
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
        case hash("scene"): case hash("CCScene"): {
            auto scene = CCScene::create();
            this->acquireVarName(child, scene);
            m_cppData << "auto " << m_varNames[scene] << " = CCScene::create();\n";
            CCDirector::sharedDirector()->pushScene(scene);
            Managed::get()->scene();
            Managed::get()->add(scene);
            this->handleEdit(child, scene);

            auto res = this->parseRecursive(child, scene);
            if (!res) return Err<>(res.error());

            m_cppData << "\n";

            return Ok<CCNode*>(scene);
        } break;

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
            std::string font = this->replaceVariables(child->Attribute("font"));
            if (!font.size()) font = "bigFont.fnt";
            auto etext = child->FirstChild();
            std::string text = "";
            if (etext && (etext = etext->ToText())) {
                text = this->replaceVariables(
                    string_utils::trim(etext->Value())
                );
            }

            auto label = CCLabelBMFont::create(text.c_str(), font.c_str());
            this->acquireVarName(child, label);
            m_cppData << "auto " << m_varNames[label] << " = CCLabelBMFont::create(\""
                << string_utils::replace(text, "\"", "\\\"") << "\", \"" << font << "\");\n";
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
            if (src) sprite = CCSprite::create(this->replaceVariables(src).c_str());
            auto frame = child->Attribute("frame");
            if (frame) sprite = CCSprite::createWithSpriteFrameName(this->replaceVariables(frame).c_str());
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
                if (src) sprite = CCScale9Sprite::create(
                    this->replaceVariables(src).c_str(), rect
                );
                frame = child->Attribute("frame");
                if (frame) sprite = CCScale9Sprite::createWithSpriteFrameName(
                    this->replaceVariables(frame).c_str(), rect
                );
            } else {
                src = child->Attribute("src");
                if (src) sprite = CCScale9Sprite::create(this->replaceVariables(src).c_str());
                frame = child->Attribute("frame");
                if (frame) sprite = CCScale9Sprite::createWithSpriteFrameName(this->replaceVariables(frame).c_str());
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
            std::string src = child->Attribute("src");
            if (!src.size()) src = this->replaceVariables("GJ_button_01.png");
            std::string font = child->Attribute("font");
            if (!font.size()) font = "bigFont.fnt";
            auto etext = child->FirstChild();
            auto tscale = child->Attribute("tscale");
            float scale = 1.f;
            if (tscale) {
                try { scale = std::stof(tscale); } catch(...) {}
            }
            std::string text = "";
            if (etext && (etext = etext->ToText())) {
                text = this->replaceVariables(string_utils::trim(etext->Value()));
            }
            if (src.size()) sprite = ButtonSprite::create(
                text.c_str(), font.c_str(), src.c_str(), scale
            );
            if (sprite) {
                this->acquireVarName(child, sprite);
                m_cppData << "auto " << m_varNames[sprite] << " = ButtonSprite::create(\""
                    << string_utils::replace(text, "\"", "\\\"") << "\", \"" << font << "\", \"" << src << "\", " << floatFormat(scale) << ");\n";
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
    auto name = std::string(child->Name());
    if (name._Starts_with("layout:")) {
        auto type = name.substr(name.find(":") + 1);
        auto pos = this->handleLayoutEdit(child, parent);
        CCNode* asNode = nullptr;
        auto asNodeAttr = child->Attribute("as");
        if (asNodeAttr) {
            auto asNodeStr = std::string(asNodeAttr);
            if (asNodeStr != "") {
                asNode = asNodeStr == "menu" ? CCMenu::create() : CCNode::create();
                asNode->setPositionX(pos.x != g_none ? pos.x : 0.f);
                asNode->setPositionY(pos.y != g_none ? pos.y : 0.f);
                pos = CCPoint { g_none, g_none };
                this->acquireVarName(child, asNode);
                m_cppData << "auto " << m_varNames[asNode] << " = "
                    << (asNodeStr == "menu" ? "CCMenu" : "CCNode")
                    <<"::create();\n"
                    << m_varNames[asNode] << "->setPosition("
                    << floatFormat(asNode->getPositionX()) << ", "
                    << floatFormat(asNode->getPositionY()) << ");\n\n";
                if (parent) {
                    parent->addChild(asNode);
                    Managed::get()->add(asNode);
                }
            }
        }
        auto res = this->parseRecursive(child, asNode ? asNode : parent);
        if (!res) return res;
        float pad = 0.f;
        auto paddingText = child->Attribute("pad");
        if (paddingText) {
            try { pad = std::stof(this->replaceVariables(paddingText)); } catch(...) {}
        }
        auto nodes = res.value();
        if (type == "row") {
            std::reverse(nodes.begin(), nodes.end());
        }
        CCSize totalSize = { 0, 0 };
        for (auto& node : nodes) {
            totalSize += node->getScaledContentSize() + CCSize { pad, pad };
        }
        if (totalSize.width > pad) totalSize.width -= pad;
        if (totalSize.height > pad) totalSize.height -= pad;

        switch (hash(type.c_str())) {
            case hash("row"): case hash("column"): {
                float caretPos = 0;
                float alignOffset = calcAlignFromString(
                    child->Attribute("align"),
                    type == "row" ? totalSize.width : totalSize.height
                );
                for (auto& node : nodes) {
                    auto caretInc = type == "row" ? 
                        node->getScaledContentSize().width :
                        node->getScaledContentSize().height;
                    caretPos += caretInc / 2;
                    auto aligned = caretPos + alignOffset;

                    const char* alignAxis;
                    const char* altAxis;
                    float alignPos;
                    float altPos;
                    float altOrg;
                    if (type == "row") {
                        alignAxis = "X";
                        altAxis = "Y";
                        alignPos = pos.x;
                        altPos = pos.y;
                        altOrg = node->getPositionY();
                    } else {
                        alignAxis = "Y";
                        altAxis = "X";
                        alignPos = pos.y;
                        altPos = pos.x;
                        altOrg = node->getPositionX();
                    }

                    if (alignPos != g_none) {
                        m_cppData << m_varNames[node] << "->setPosition" << alignAxis << "("
                            << m_varNames[parent] + "_layout" << alignAxis
                            << (aligned > 0 ? " + " + floatFormat(aligned) : (aligned < 0 ? " - " + floatFormat(-aligned) : ""))
                            << ");\n";
                    } else if (aligned) {
                        m_cppData << m_varNames[node] << "->setPosition" << alignAxis
                            << "(" << floatFormat(aligned) << ");\n";
                    }
                    if (altPos != g_none) {
                        m_cppData << m_varNames[node] << "->setPosition" << altAxis
                            << "(" << m_varNames[parent] + "_layout" << altAxis
                            << (altOrg > 0 ? " + " + floatFormat(altOrg) :
                                (altOrg < 0 ? " - " + floatFormat(-altOrg) : "")
                            ) << ");\n";
                    }

                    if (type == "row") {
                        node->setPosition(
                            (alignPos == g_none ? 0.f : alignPos) + aligned,
                            (altPos != g_none ? altPos : .0f) + altOrg
                        );
                    } else {
                        node->setPosition(
                            (altPos != g_none ? altPos : .0f) + altOrg,
                            (alignPos == g_none ? 0.f : alignPos) + aligned
                        );
                    }

                    caretPos += caretInc / 2 + pad;
                }
            } break;
        }
        
        m_cppData << "\n";
        if (asNode) {
            float coveredLeft = 0.f;
            float coveredRight = 0.f;
            float coveredTop = 0.f;
            float coveredBottom = 0.f;
            for (auto& node : nodes) {
                auto nleft  = node->getPositionX() - node->getScaledContentSize().width / 2;
                auto nright = node->getPositionX() + node->getScaledContentSize().width / 2;
                auto nbottom= node->getPositionY() - node->getScaledContentSize().height/ 2;
                auto ntop   = node->getPositionY() + node->getScaledContentSize().height/ 2;
                if (nleft < coveredLeft) coveredLeft = nleft;
                if (nright > coveredRight) coveredRight = nright;
                if (nbottom < coveredBottom) coveredBottom = nbottom;
                if (ntop > coveredTop) coveredTop = ntop;
            }
            CCSize coveredSize = {
                fabsf(coveredRight - coveredLeft),
                fabsf(coveredTop - coveredBottom)
            };
            asNode->setContentSize(coveredSize);
            if (parent) {
                m_cppData << m_varNames[asNode] << "->setContentSize({ "
                    << floatFormat(coveredSize.width) << ","
                    << floatFormat(coveredSize.height)
                    << " });\n" << m_varNames[parent] << "->addChild("
                    << m_varNames[asNode] << ");\n";
            }
            m_cppData << "\n";
            return Ok<std::vector<CCNode*>>({ asNode });
        }
        return Ok<>(res.value());
    } else {
        auto res = this->createNode(child, parent);
        if (!res) return Err<>(res.error());
        return Ok<std::vector<CCNode*>>({ res.value() });
    }
    return Err<>("Unknown error");
}

Result<std::vector<CCNode*>> GDML::createCustom(tinyxml2::XMLElement* child, CCNode* node) {
    auto name = std::string(child->Name());
    if (name._Starts_with("define:")) {
        m_cppData << "\n";
        
        auto type = name.substr(name.find(":") + 1);
        if (m_models.count(type)) {
            return Err<>("\"" + type + "\" already defined");
        }
        m_models[type] = child;

        return Ok<std::vector<CCNode*>>({});

    } else if (name._Starts_with("create:")) {
        
        auto type = name.substr(name.find(":") + 1);
        if (!m_models.count(type)) {
            return Err<>("\"" + type + "\" not defined");
        }
        return this->parseRecursive(m_models[type], node);

    } else {
        return this->createLayout(child, node);
    }
    return Err<>("Unknown error");
}

Result<std::vector<CCNode*>> GDML::createVariable(tinyxml2::XMLElement* child, CCNode* node) {
    auto name = std::string(child->Name());

    auto setVar = [&](Variable::Type type) -> Result<std::vector<CCNode*>> {
        auto varName = name.substr(name.find(":") + 1);
        if (m_models.count(varName)) {
            return Err<>("\"" + varName + "\" already defined");
        }
        try {
            Variable var;
            var.m_type = type;
            auto c = child->FirstChild();
            if (c) {
                auto t = c->ToText();
                if (!t) return Err<>("\"" + varName + "\": non-text child found");
                switch (type) {
                    case Variable::Int: var.m_value = std::stoi(t->Value()); break;
                    case Variable::Float: var.m_value = std::stof(t->Value()); break;
                    case Variable::String: var.m_value = std::string(t->Value()); break;
                    default: return Err<>("\"" + varName + "\": invalid type");
                }
            } else {
                switch (type) {
                    case Variable::Int: var.m_value = 0; break;
                    case Variable::Float: var.m_value = 0; break;
                    case Variable::String: var.m_value = ""; break;
                    default: return Err<>("\"" + varName + "\": invalid type");
                }
            }
            m_variables[varName] = var;
        } catch(...) {
            return Err<>("\"" + varName + "\": default value does not match type");
        }
        return Ok<std::vector<CCNode*>>({});
    };

    if (name._Starts_with("int:")) {

        return setVar(Variable::Int);

    } else if (name._Starts_with("float:")) {

        return setVar(Variable::Float);

    } else if (name._Starts_with("string:")) {

        return setVar(Variable::String);

    } else {
        return this->createCustom(child, node);
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
                    auto res = this->createVariable(child, node);
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
    m_models.clear();
    m_variables.clear();

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
