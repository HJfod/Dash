#include <GDML.hpp>

using namespace std::string_literals;
using namespace gdml;

#define UNWRAP(_type, _var, _exp) \
    _type _var; {\
        auto cached##_var = _exp;\
        if (!cached##_var) return Err<>(cached##_var.error());\
        _var = cached##_var.value();\
    }

#define UNWRAP_TEXT(_var) \
    UNWRAP(std::string, _var, this->getTextValue(data.m_element))
#define UNWRAP_ATTR_(_var, _child, _attr, _def) \
    UNWRAP(std::string, _var, this->getAttribute(_child, _attr, _def))
#define UNWRAP_ATTR(_var, _attr, _def) \
    UNWRAP_ATTR_(_var, data.m_element, _attr, _def)
#define UNWRAP_CALCED(_var, _attr, _def) \
    UNWRAP(float, _var, this->getAttributeCalced(child, node, parent, _attr, _def))

GDML* GDML::get() {
    static auto inst = new GDML;
    return inst;
}

GDML::GDML() {
    this->registerNode("CCLabelBMFont", [&](CreateData const& data) -> Result<CCNode*> {
        UNWRAP_TEXT(text);
        UNWRAP_ATTR(font, "font", "bigFont.fnt");
        return Ok<>(CCLabelBMFont::create(text.c_str(), font.c_str()));
    }, [&](auto) { return CCLabelBMFont::create(); });
    this->registerAliasForNode("label", "CCLabelBMFont");

    this->registerNode("CCNode", [&](CreateData const& data) -> Result<CCNode*> {
        return Ok<>(CCNode::create());
    });
    this->registerAliasForNode("node", "CCNode");

    this->registerNode("CCScene", [&](CreateData const& data) -> Result<CCNode*> {
        return Ok<>(CCScene::create());
    });
    this->registerAliasForNode("scene", "CCScene");

    this->registerNode("CCLayer", [&](CreateData const& data) -> Result<CCNode*> {
        return Ok<>(CCLayer::create());
    });
    this->registerAliasForNode("layer", "CCLayer");

    this->registerNode("CCMenu", [&](CreateData const& data) -> Result<CCNode*> {
        return Ok<>(CCMenu::create());
    });
    this->registerAliasForNode("menu", "CCMenu");

    this->registerNode("CCSprite", [&](CreateData const& data) -> Result<CCNode*> {
        auto hasSrc = this->hasAttribute(data.m_element, "src");
        auto hasFrame = this->hasAttribute(data.m_element, "frame");
        if (hasSrc && hasFrame) {
            return Err<>("Both \"src\" and \"frame\" defined, unable to pick one");
        }
        if (hasSrc) {
            UNWRAP_ATTR(src, "src", "");
            auto spr = CCSprite::create(src.c_str());
            if (spr) {
                return Ok<>(spr);
            }
        }
        if (hasFrame) {
            UNWRAP_ATTR(frame, "frame", "");
            auto spr = CCSprite::createWithSpriteFrameName(frame.c_str());
            if (spr) {
                return Ok<>(spr);
            }
        }
        return Ok<>(nullptr);
    }, [&](auto) { return CCSprite::create(); });
    this->registerAliasForNode("sprite", "CCSprite");

    this->registerNode("CCScale9Sprite", [&](CreateData const& data) -> Result<CCNode*> {
        auto hasSrc = this->hasAttribute(data.m_element, "src");
        auto hasFrame = this->hasAttribute(data.m_element, "frame");
        auto hasRect = this->hasAttribute(data.m_element, "rect");
        if (hasSrc && hasFrame) {
            return Err<>("Both \"src\" and \"frame\" defined, unable to pick one");
        }
        CCRect rect;
        if (hasRect) {
            auto rval = this->getAttribute(data.m_element, "rect");
            if (!rval) return Err<>(rval.error());
            auto parse = parseRect(rval.value());
            if (!parse) return Err<>(parse.error());
            rect = parse.value();
        }
        if (hasSrc) {
            UNWRAP_ATTR(src, "src", "");
            auto spr = hasRect ?
                CCScale9Sprite::create(src.c_str(), rect) :
                CCScale9Sprite::create(src.c_str());
            if (spr) {
                return Ok<>(spr);
            }
        }
        if (hasFrame) {
            UNWRAP_ATTR(frame, "frame", "");
            auto spr = hasRect ?
                CCScale9Sprite::createWithSpriteFrameName(frame.c_str(), rect) : 
                CCScale9Sprite::createWithSpriteFrameName(frame.c_str());
            if (spr) {
                return Ok<>(spr);
            }
        }
        return Ok<>(nullptr);
    }, [&](auto) { return CCScale9Sprite::create(""); });
    this->registerAliasForNode("ssprite", "CCScale9Sprite");

    this->registerNode("ButtonSprite", [&](CreateData const& data) -> Result<CCNode*> {
        UNWRAP_TEXT(text);
        UNWRAP_ATTR(font, "font", "bigFont.fnt");
        UNWRAP_ATTR(src, "src", "GJ_button_01.png");
        UNWRAP(float, tscale, this->getAttributeCalced(
            data.m_element, nullptr, data.m_parent, "tscale", 1.f
        ));
        return Ok<>(ButtonSprite::create(text.c_str(), font.c_str(), src.c_str(), tscale));
    }, [&](auto) { return ButtonSprite::create(""); });
    this->registerAliasForNode("bsprite", "ButtonSprite");

    this->registerNode("CCMenuItemSpriteExtra", [&](CreateData const& data) -> Result<CCNode*> {
        UNWRAP_ATTR(callbackName, "callback", "");

        SEL_MenuHandler callback = nullptr;
        if (callbackName.size()) {
            if (!m_callbacks.count(callbackName)) {
                return Err<>("Callback \"" + callbackName + "\" not found");
            }
            callback = m_callbacks.at(callbackName);
        }

        UNWRAP_TEXT(text);
        UNWRAP_ATTR(font, "font", "bigFont.fnt");
        UNWRAP_ATTR(bg, "bg", "GJ_button_01.png");

        auto child = data.m_element->FirstChildElement();
        if (!child) {
            if (!text.size()) {
                return Err<>(
                    "Button has no child to use as sprite and no text "
                    "content to create a default ButtonSprite from"
                );
            }
            auto spr = ButtonSprite::create(text.c_str(), font.c_str(), bg.c_str());
            if (!spr) return Err<>("Creating default ButtonSprite for button failed");
            return Ok<>(CCMenuItemSpriteExtra::create(spr, data.m_parent, callback));
        }
        auto spr = this->createNode(child, nullptr);
        if (!spr) return Err<>(spr.error());
        data.m_element->DeleteChild(child);
        return Ok<>(CCMenuItemSpriteExtra::create(spr.value(), data.m_parent, callback));

    }, [&](CreateData const& data) {
        return CCMenuItemSpriteExtra::create(CCNode::create(), data.m_parent, nullptr);
    });
    this->registerAliasForNode("button", "CCMenuItemSpriteExtra");
}

Result<> GDML::registerAliasForNode(std::string const& newTag, std::string const& from) {
    if (m_nodes.count(from)) {
        if (!m_nodes.count(newTag)) {
            m_nodes[newTag] = m_nodes[from];
            return Ok<>();
        }
        return Err<>("Tag <" + newTag + "> already defined");
    }
    return Err<>("Tag <" + from + "> not found");
}

Result<std::string> GDML::replaceVariables(std::string const& text) {
    try {
        return Ok<>(fmt::vformat(text, m_variables));
    } catch(std::exception& e) {
        return Err<>("Formatting error: "s + e.what());
    }
}

Result<std::string> GDML::process(std::string const& text) {
    auto s = this->replaceVariables(text);
    return s;
}

Result<std::string> GDML::getTextValue(tinyxml2::XMLElement* child, std::string const& def) {
    auto text = child->GetText();
    if (!text) return Ok<>(def);
    return this->process(text);
}

bool GDML::hasAttribute(tinyxml2::XMLElement* child, std::string const& attr) {
    return child->Attribute(attr.c_str());
}

Result<std::string> GDML::getAttribute(
    tinyxml2::XMLElement* child,
    std::string const& attr,
    std::string const& def
) {
    auto raw = child->Attribute(attr.c_str());
    if (!raw) return Ok<>(def);
    return this->process(raw);
}

Result<float> GDML::getAttributeCalced(
    tinyxml2::XMLElement* child,
    CCNode* node,
    CCNode* parent,
    std::string const& attr,
    float def
) {
    auto raw = child->Attribute(attr.c_str());
    if (!raw) return Ok<float>(def);
    return this->processMath(node, parent, raw);
}

Result<> GDML::parseEdit(tinyxml2::XMLElement* child, CCNode* node, CCNode* parent) {
    #define UNWRAP_EDIT(_attr, _func) \
        { UNWRAP_CALCED(_func, _attr, node->get##_func()); node->set##_func(_func); }

    auto rgba = dynamic_cast<CCRGBAProtocol*>(node);
    auto label = dynamic_cast<CCLabelProtocol*>(node);

    UNWRAP_EDIT("x", PositionX);
    UNWRAP_EDIT("y", PositionY);
    UNWRAP_EDIT("z", ZOrder);
    UNWRAP_EDIT("scale", Scale);
    UNWRAP_EDIT("scale-x", ScaleX);
    UNWRAP_EDIT("scale-y", ScaleY);
    UNWRAP_EDIT("rotation", Rotation);
    UNWRAP_EDIT("rotation-x", RotationX);
    UNWRAP_EDIT("rotation-y", RotationY);
    UNWRAP_EDIT("skew-x", SkewX);
    UNWRAP_EDIT("skew-y", SkewY);

    UNWRAP_ATTR_(csize, child, "size", "");
    if (csize.size()) {
        if (string_utils::contains(csize, ':')) {
            auto width = this->processMath(node, parent, csize.substr(0, csize.find(':')));
            auto height = this->processMath(node, parent, csize.substr(csize.find(':') + 1));
            if (!width) return Err<>("Content size width is malformed: " + width.error());
            if (!height) return Err<>("Content size height is malformed: " + height.error());
            node->setContentSize({ width.value(), height.value() });
        } else {
            return Err<>("Content size is not in format [w:h]");
        }
    }

    UNWRAP_ATTR_(anchor, child, "anchor", "");
    if (anchor.size()) {
        if (string_utils::contains(anchor, ',')) {
            auto x = this->processMath(node, parent, anchor.substr(0, anchor.find(',')));
            auto y = this->processMath(node, parent, anchor.substr(anchor.find(',') + 1));
            if (!x) return Err<>("Anchor point x is malformed: " + x.error());
            if (!y) return Err<>("Anchor point y is malformed: " + y.error());
            node->setAnchorPoint({ x.value(), y.value() });
        } else {
            return Err<>("Anchor point is not in format [x:y]");
        }
    }

    if (this->hasAttribute(child, "color")) {
        if (!rgba) return Err<>("\"color\" used on non-RGBA node");
        auto colorRes = this->getAttribute(child, "color");
        if (!colorRes) return Err<>(colorRes.error());
        if (colorRes.value().size()) {
            auto color = parseColor(colorRes.value());
            if (!color) return Err<>(color.error());
            rgba->setColor(color.value());
        }
    }

    if (this->hasAttribute(child, "opacity")) {
        if (!rgba) return Err<>("\"opacity\" used on non-RGBA node");
        auto opacityRes = this->getAttribute(child, "opacity");
        if (!opacityRes) return Err<>(opacityRes.error());
        if (opacityRes.value().size()) {
            try {
                auto opacity = std::stoi(opacityRes.value());
                if (opacity < 0 || opacity > 255)
                    return Err<>("Opacity outside valid range of [0-255]");
                rgba->setOpacity(opacity);
            } catch(std::exception& e) {
                return Err<>("Error setting opacity: "s + e.what());
            }
        }
    }

    if (this->hasAttribute(child, "text")) {
        if (!label) return Err<>("\"text\" used on non-label");
        auto textRes = this->getAttribute(child, "text");
        if (!textRes) return Err<>(textRes.error());
        label->setString(textRes.value().c_str());
    }

    return Ok<>();
}

Result<CCNode*> GDML::createNode(tinyxml2::XMLElement* child, CCNode* parent) {
    std::string name = child->Name();
    std::string subtype = "";
    if (string_utils::contains(name, ":")) {
        subtype = name.substr(name.find(":") + 1);
        name = name.substr(0, name.find(":"));
    }
    if (this->m_nodes.count(name)) {
        CreateData params { subtype, child, parent };
        auto res = this->m_nodes.at(name)(params);
        if (!res) return Err<>(res.error());
        auto node = res.value();
        auto fallback = child->FirstChildElement("fallback");
        if (fallback && !m_fallbackNodes.count(name)) {
            return Err<>(
                "<fallback> used on a node without fallback function defined; "
                "provide the callback in GDML::registerNode"
            );
        }
        if (!node) {
            if (fallback) {
                node = m_fallbackNodes.at(name)(params);
                if (!node) {
                    return Err<>(
                        "Fallback function returned nullptr, something's "
                        "gone very very wrong. Your game will either "
                        "crash soon or the mod you're using sucks"
                    );
                }
                for (auto child = fallback->FirstChildElement(); child; child = child->NextSiblingElement()) {
                    auto res = this->parseRecursive(child, node);
                    if (!res) return Err<>(res.error());
                }
            } else {
                return Err<>(
                    "Created node was null and no <fallback> was specified"
                );
            }
        }
        m_created.push_back(node);
        child->DeleteChild(fallback);
        auto edit = this->parseEdit(child, node, parent);
        if (!edit) return Err<>(edit.error());
        auto nodeName = this->getAttribute(child, "name");
        if (!nodeName) return Err<>(nodeName.error());
        m_metadata.insert({ node, {
            nodeName.value()
        } });
        m_nodeNames.insert({ nodeName.value(), node });
        if (parent) parent->addChild(node);
        return Ok<>(node);
    } else {
        return Err<>(
            "Unknown node <" + name + ">; if you want to create a custom "
            "node, make sure to call GDML::registerNode or use the "
            "GDML_REGISTER macro"
        );
    }
}

Result<GDML::ParsedNodes> GDML::parseRecursive(tinyxml2::XMLElement* elem, CCNode* parent) {
    ParsedNodes result;
    for (auto child = elem->FirstChildElement(); child; child = child->NextSiblingElement()) {
        std::string name = child->Name();
        std::string subtype = "";
        if (string_utils::contains(name, ":")) {
            subtype = name.substr(name.find(":") + 1);
            name = name.substr(0, name.find(":"));
        }
        if (!this->m_nodes.count(name)) {
            switch (hash(name.c_str())) {
                case hash("goto"): {
                    if (child->NoChildren()) {
                        return Err<>("<goto> needs to have a <scene> child");
                    }
                    if (child->FirstChild() != child->LastChild()) {
                        return Err<>("<goto> can only have one child");
                    }
                    auto res = this->parseRecursive(child, parent);
                    if (!res) return Err<>(res.error());
                    auto scene = dynamic_cast<CCScene*>(res.value().at(0));
                    if (!scene) {
                        return Err<>("Child of <goto> must be a CCScene");
                    }
                    auto fade = CCTransitionFade::create(.5f, scene);
                    CCDirector::sharedDirector()->replaceScene(fade);
                    continue;
                } break;
            }
        }
        auto node = this->createNode(child, parent);
        if (!node) return Err<>(node.error());
        result.push_back(node.value());
        auto res = this->parseRecursive(child, node.value());
        if (!res) return Err<>(res.error());
    }
    return Ok<ParsedNodes>(result);
}

Result<> GDML::registerCallback(std::string const& id, SEL_MenuHandler handler, bool override) {
    if (m_callbacks.count(id) && !override) {
        return Err<>("Callback \"" + id + "\" already defined");
    }
    m_callbacks[id] = handler;
    return Ok<>();
}

SEL_MenuHandler GDML::getCallback(std::string const& id) const {
    if (!m_callbacks.count(id)) return nullptr;
    return m_callbacks.at(id);
}

void GDML::clearCallbacks() {
    m_callbacks.clear();
}

Result<> GDML::registerNode(
    std::string const& tag,
    NodeCreateFunc func,
    FallbackCreateFunc fallbackFunc,
    bool override
) {
    if (m_nodes.count(tag) && !override) {
        return Err<>("Tag <" + tag + "> is already defined");
    }
    m_nodes[tag] = func;
    if (fallbackFunc) {
        m_fallbackNodes[tag] = fallbackFunc;
    } else {
        m_fallbackNodes.erase(tag);
    }
    return Ok<>();
}

Result<> GDML::registerNode(
    std::string const& tag,
    NodeCreateFunc func,
    bool override
) {
    return this->registerNode(tag, func, nullptr, override);
}

void GDML::clear() {}

Result<GDML::ParsedNodes> GDML::parseDoc(tinyxml2::XMLDocument* doc, ParseOptions const& options) {
    m_created.clear();
    auto gdml = doc->FirstChildElement("gdml");
    if (gdml) {
        auto res = this->parseRecursive(gdml, options.m_parent);
        if (!res) {
            for (auto& node : m_created) {
                node->removeFromParent();
            }
            m_created.clear();
        }
        return res;
    }
    GDML::ParsedNodes nodes;
    for (auto child = doc->FirstChildElement(); child; child->NextSiblingElement()) {
        auto res = this->parseRecursive(gdml, options.m_parent);
        if (!res) {
            for (auto& node : m_created) {
                node->removeFromParent();
            }
            m_created.clear();
            return res;
        }
        vector_utils::push(nodes, res.value());
    }
    return Ok<GDML::ParsedNodes>(nodes);
}

Result<GDML::ParsedNodes> GDML::parseString(std::string const& data, ParseOptions const& options) {
    tinyxml2::XMLDocument doc;
    auto r = doc.Parse(data.c_str(), data.size());
    if (r != tinyxml2::XML_SUCCESS) {
        return Err<>("Parse error: "s + parseErrorAsString(r) + " (" + std::to_string(r) + ")");
    }
    return this->parseDoc(&doc, options);
}

Result<GDML::ParsedNodes> GDML::parseFile(ghc::filesystem::path const& path, ParseOptions const& options) {
    auto read = file_utils::readString(path);
    if (!read) {
        return Err<>(read.error());
    }
    if (!read.value().size()) return Ok<GDML::ParsedNodes>({});
    return this->parseString(read.value());
}

Result<GDML::ParsedNodes> GDML::parse(std::string const& data, ParseOptions const& options) {
    return GDML::get()->parseString(data, options);
}

CCNode* GDML::getNodeByName(std::string const& name) const {
    if (!m_nodeNames.count(name)) return nullptr;
    return m_nodeNames.at(name);
}

const char* GDML::parseErrorAsString(tinyxml2::XMLError error) {
    switch (error) {
        case tinyxml2::XML_SUCCESS: return "XML_SUCCESS";
        case tinyxml2::XML_NO_ATTRIBUTE: return "XML_NO_ATTRIBUTE";
        case tinyxml2::XML_WRONG_ATTRIBUTE_TYPE: return "XML_WRONG_ATTRIBUTE_TYPE";
        case tinyxml2::XML_ERROR_FILE_NOT_FOUND: return "XML_ERROR_FILE_NOT_FOUND";
        case tinyxml2::XML_ERROR_FILE_COULD_NOT_BE_OPENED: return "XML_ERROR_FILE_COULD_NOT_BE_OPENED";
        case tinyxml2::XML_ERROR_FILE_READ_ERROR: return "XML_ERROR_FILE_READ_ERROR";
        case tinyxml2::XML_ERROR_ELEMENT_MISMATCH: return "XML_ERROR_ELEMENT_MISMATCH";
        case tinyxml2::XML_ERROR_PARSING_ELEMENT: return "XML_ERROR_PARSING_ELEMENT";
        case tinyxml2::XML_ERROR_PARSING_ATTRIBUTE: return "XML_ERROR_PARSING_ATTRIBUTE";
        case tinyxml2::XML_ERROR_IDENTIFYING_TAG: return "XML_ERROR_IDENTIFYING_TAG";
        case tinyxml2::XML_ERROR_PARSING_TEXT: return "XML_ERROR_PARSING_TEXT";
        case tinyxml2::XML_ERROR_PARSING_CDATA: return "XML_ERROR_PARSING_CDATA";
        case tinyxml2::XML_ERROR_PARSING_COMMENT: return "XML_ERROR_PARSING_COMMENT";
        case tinyxml2::XML_ERROR_PARSING_DECLARATION: return "XML_ERROR_PARSING_DECLARATION";
        case tinyxml2::XML_ERROR_PARSING_UNKNOWN: return "XML_ERROR_PARSING_UNKNOWN";
        case tinyxml2::XML_ERROR_EMPTY_DOCUMENT: return "XML_ERROR_EMPTY_DOCUMENT";
        case tinyxml2::XML_ERROR_MISMATCHED_ELEMENT: return "XML_ERROR_MISMATCHED_ELEMENT";
        case tinyxml2::XML_ERROR_PARSING: return "XML_ERROR_PARSING";
        case tinyxml2::XML_CAN_NOT_CONVERT_TEXT: return "XML_CAN_NOT_CONVERT_TEXT";
        case tinyxml2::XML_NO_TEXT_NODE: return "XML_NO_TEXT_NODE";
    }
    return "Unknown";
}

Result<GDML::ParsedNodes> operator"" _gdml(const char* str, size_t) {
    return GDML::parse(str);
}
