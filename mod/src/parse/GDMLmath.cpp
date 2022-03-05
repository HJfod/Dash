#include "GDML.hpp"
#include "../utils/exprtk.hpp"

void normalizeExp(std::string& str) {
    string_utils::replaceIP(str, "->", "_ARROW_");
    string_utils::replaceIP(str, "/", " / ");
    string_utils::replaceIP(str, "*", " * ");
    string_utils::replaceIP(str, "+", " + ");
    string_utils::replaceIP(str, "-", " - ");
    while (string_utils::contains(str, "  ")) {
        string_utils::replaceIP(str, "  ", " ");
    }
    string_utils::replaceIP(str, "_ARROW_", "->");
}

Result<> GDML::handleEdit(tinyxml2::XMLElement* elem, cocos2d::CCNode* node) {
    auto winSize = CCDirector::sharedDirector()->getWinSize();

    Managed::get()->touch(node);

    auto rgba = dynamic_cast<CCRGBAProtocol*>(node);
    auto label = dynamic_cast<CCLabelProtocol*>(node);
    auto sprite = dynamic_cast<CCSprite*>(node);

    auto replaceExpShortHand = [this, node](std::string str) -> std::string {
        // because you can't just replace "w" with something else when 
        // nearly every prior replacement has w in it
        string_utils::replaceIP(str, "px", "_gaagaa");
        string_utils::replaceIP(str, "py", "_googoo");
        string_utils::replaceIP(str, "pw", "_guuguu");
        string_utils::replaceIP(str, "ph", "_geegee");
        string_utils::replaceIP(str, "ww", "_gjjgjj");
        string_utils::replaceIP(str, "wh", "_giigii");
        string_utils::replaceIP(str, "sw", "_grrgrr");
        string_utils::replaceIP(str, "sh", "_ghhghh");
        string_utils::replaceIP(str, "tx", "_gggggg");
        string_utils::replaceIP(str, "ty", "_g88g88");
        string_utils::replaceIP(str, "tw", "_g00g00");
        string_utils::replaceIP(str, "th", "_g22g22");
        string_utils::replaceIP(str, "x",  "_gkkgkk");
        string_utils::replaceIP(str, "y",  "_gllgll");
        string_utils::replaceIP(str, "w",  "_gnngnn");
        string_utils::replaceIP(str, "h",  "_gppgpp");

        string_utils::replaceIP(str, "_gaagaa", m_varNames[node->getParent()] + "->getPositionX()");
        string_utils::replaceIP(str, "_googoo", m_varNames[node->getParent()] + "->getPositionY()");
        string_utils::replaceIP(str, "_guuguu", m_varNames[node->getParent()] + "->getContentSize().width");
        string_utils::replaceIP(str, "_geegee", m_varNames[node->getParent()] + "->getContentSize().height");
        string_utils::replaceIP(str, "_gjjgjj", "winSize.width");
        string_utils::replaceIP(str, "_giigii", "winSize.height");
        string_utils::replaceIP(str, "_grrgrr", m_varNames[node] + "->getScaledContentSize().width");
        string_utils::replaceIP(str, "_ghhghh", m_varNames[node] + "->getScaledContentSize().height");
        string_utils::replaceIP(str, "_gkkgkk", m_varNames[node] + "->getPositionX()");
        string_utils::replaceIP(str, "_gllgll", m_varNames[node] + "->getPositionY()");
        string_utils::replaceIP(str, "_gnngnn", m_varNames[node] + "->getContentSize().width");
        string_utils::replaceIP(str, "_gppgpp", m_varNames[node] + "->getContentSize().height");
        string_utils::replaceIP(str, "_gggggg", m_varNames[node] + "->getTextureRect().origin.x");
        string_utils::replaceIP(str, "_g88g88", m_varNames[node] + "->getTextureRect().origin.y");
        string_utils::replaceIP(str, "_g00g00", m_varNames[node] + "->getTextureRect().size.width");
        string_utils::replaceIP(str, "_g22g22", m_varNames[node] + "->getTextureRect().size.height");
        normalizeExp(str);
        return str;
    };

    exprtk::symbol_table<float> table;
    table.add_constant("x", node->getPositionX());
    table.add_constant("y", node->getPositionY());
    table.add_constant("w", node->getContentSize().width);
    table.add_constant("h", node->getContentSize().height);
    table.add_constant("sw", node->getScaledContentSize().width);
    table.add_constant("sh", node->getScaledContentSize().height);
    table.add_constant("ww", winSize.width);
    table.add_constant("wh", winSize.height);
    for (auto& [var, val] : m_variables) {
        if (val.m_type == Variable::Float) {
            table.add_constant(var, std::any_cast<float>(val.m_value));
        }
        if (val.m_type == Variable::Int) {
            table.add_constant(var, std::any_cast<int>(val.m_value));
        }
    }
    if (node->getParent()) {
        table.add_constant("px", node->getParent()->getPositionX());
        table.add_constant("py", node->getParent()->getPositionY());
        table.add_constant("pw", node->getParent()->getContentSize().width);
        table.add_constant("ph", node->getParent()->getContentSize().height);
    }
    if (sprite) {
        table.add_constant("tx", sprite->getTextureRect().origin.x);
        table.add_constant("ty", sprite->getTextureRect().origin.y);
        table.add_constant("tw", sprite->getTextureRect().size.width);
        table.add_constant("th", sprite->getTextureRect().size.height);
    }

    exprtk::expression<float> exp;
    exp.register_symbol_table(table);

    exprtk::parser<float> parser;

    #define EDIT_NAME(f) edit_ ## f

    #define RESOLVE_EXP(_var_, _val_, _type_) \
        {auto expVal = this->replaceVariables(_val_);\
        if (parser.compile(_val_, exp)) {\
            _var_ = static_cast<_type_>(exp.value());\
        } else {\
            Log::get() << "Error parsing math expression \"" #_var_ "\": " << parser.error();\
        }}

    #define ADD_EDIT(_attr_, _func_, _type_) \
        auto EDIT_NAME(_func_) = elem->Attribute(_attr_); \
        if (EDIT_NAME(_func_)) {\
            auto val = this->replaceVariables(EDIT_NAME(_func_));\
            if (parser.compile(val, exp)) {\
                node->_func_(static_cast<_type_>(exp.value()));\
                m_cppData << m_varNames[node] << "->" #_func_ "(" << replaceExpShortHand(val) << ");\n";\
            } else {\
                Log::get() << "Error parsing math expression for \"" _attr_ "\": " << parser.error();\
            }\
        }

    ADD_EDIT("x", setPositionX, float);
    ADD_EDIT("y", setPositionY, float);
    ADD_EDIT("z", setZOrder,    int);
    ADD_EDIT("scale", setScale, float);
    ADD_EDIT("scale-x", setScaleX, float);
    ADD_EDIT("scale-y", setScaleY, float);
    ADD_EDIT("rotate", setRotation, float);
    ADD_EDIT("rotate-x", setRotationX, float);
    ADD_EDIT("rotate-y", setRotationY, float);
    ADD_EDIT("skew-x", setSkewX, float);
    ADD_EDIT("skew-y", setSkewY, float);

    auto sizeEdit = elem->Attribute("size");
    if (sizeEdit) {
        auto val = std::string(sizeEdit);
        try {
            if (string_utils::contains(val, ',')) {
                CCSize size = node->getContentSize();
                RESOLVE_EXP(size.width, val.substr(0, val.find(',')), float);
                RESOLVE_EXP(size.height, val.substr(val.find(',') + 1), float);
                node->setContentSize(size);
                m_cppData << m_varNames[node] << "->setContentSize({ " << val << " });\n";
            } else {
                Log::get() << "Invalid format for size";
            }
        } catch(...) {}
    }

    auto anchorEdit = elem->Attribute("anchor");
    if (anchorEdit) {
        auto val = std::string(anchorEdit);
        try {
            if (string_utils::contains(val, ',')) {
                CCPoint point = node->getAnchorPoint();
                RESOLVE_EXP(point.x, val.substr(0, val.find(',')), float);
                RESOLVE_EXP(point.y, val.substr(val.find(',') + 1), float);
                node->setContentSize(point);
                m_cppData << m_varNames[node] << "->setAnchorPoint({ " << val << " });\n";
            } else {
                Log::get() << "Invalid format for anchor";
            }
        } catch(...) {}
    }

    auto colorEdit = elem->Attribute("color");
    if (colorEdit) {
        try {
            if (rgba) {
                auto color = parseColor(this->replaceVariables(colorEdit));
                if (color) {
                    rgba->setColor(color.value());
                    m_cppData << m_varNames[node] << "->setColor("
                        << ccColor3BToCppString(color.value()) << ");\n";
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
                rgba->setOpacity(static_cast<GLubyte>(std::stoi(
                    this->replaceVariables(opacityEdit)
                )));
                m_cppData << m_varNames[node] << "->setOpacity("
                    << opacityEdit << ");\n";
            } else {
                Log::get() << "opacity called on non-colorable node";
            }
        } catch(...) {}
    }

    auto textEdit = elem->Attribute("text");
    if (textEdit) {
        try {
            if (label) {
                label->setString(this->replaceVariables(textEdit).c_str());
                m_cppData << m_varNames[node] << "->setString(\""
                    << string_utils::replace(textEdit, "\"", "\\\"") << "\");\n";
            } else {
                Log::get() << "text called on non-textable node";
            }
        } catch(...) {}
    }

    return Ok<>();
}

CCPoint GDML::handleLayoutEdit(tinyxml2::XMLElement* elem, cocos2d::CCNode* node) {
    auto winSize = CCDirector::sharedDirector()->getWinSize();

    Managed::get()->touch(node);

    auto rgba = dynamic_cast<CCRGBAProtocol*>(node);
    auto label = dynamic_cast<CCLabelProtocol*>(node);

    auto replaceExpShortHand = [this, node](std::string str) -> std::string {
        // because you can't just replace "w" with something else when 
        // nearly every prior replacement has w in it
        string_utils::replaceIP(str, "px", "_gaagaa");
        string_utils::replaceIP(str, "py", "_googoo");
        string_utils::replaceIP(str, "pw", "_guuguu");
        string_utils::replaceIP(str, "ph", "_geegee");
        string_utils::replaceIP(str, "ww", "_gjjgjj");
        string_utils::replaceIP(str, "wh", "_giigii");
        string_utils::replaceIP(str, "sw", "_grrgrr");
        string_utils::replaceIP(str, "sh", "_ghhghh");

        string_utils::replaceIP(str, "_gaagaa", m_varNames[node] + "->getPositionX()");
        string_utils::replaceIP(str, "_googoo", m_varNames[node] + "->getPositionY()");
        string_utils::replaceIP(str, "_guuguu", m_varNames[node] + "->getContentSize().width");
        string_utils::replaceIP(str, "_geegee", m_varNames[node] + "->getContentSize().height");
        string_utils::replaceIP(str, "_gjjgjj", "winSize.width");
        string_utils::replaceIP(str, "_giigii", "winSize.height");
        string_utils::replaceIP(str, "_grrgrr", m_varNames[node] + "->getScaledContentSize().width");
        string_utils::replaceIP(str, "_ghhghh", m_varNames[node] + "->getScaledContentSize().height");
        normalizeExp(str);
        return str;
    };

    exprtk::symbol_table<float> table;
    table.add_constant("ww", winSize.width);
    table.add_constant("wh", winSize.height);
    table.add_constant("px", node->getPositionX());
    table.add_constant("py", node->getPositionY());
    table.add_constant("pw", node->getContentSize().width);
    table.add_constant("ph", node->getContentSize().height);
    table.add_constant("sw", node->getScaledContentSize().width);
    table.add_constant("sh", node->getScaledContentSize().height);

    exprtk::expression<float> exp;
    exp.register_symbol_table(table);

    exprtk::parser<float> parser;

    CCPoint res = { g_none, g_none };

    auto xEdit = elem->Attribute("x");
    if (xEdit) {
        auto val = std::string(xEdit);
        if (parser.compile(val, exp)) {
            res.x = exp.value();
            m_cppData << "auto " << m_varNames[node] << "_layoutX = " << replaceExpShortHand(val) << ";\n";
        } else {
            Log::get() << "Error parsing math for X pos";
        }
    }

    auto yEdit = elem->Attribute("y");
    if (yEdit) {
        auto val = std::string(yEdit);
        if (parser.compile(val, exp)) {
            res.y = exp.value();
            m_cppData << "auto " << m_varNames[node] << "_layoutY = " << replaceExpShortHand(val) << ";\n";
        } else {
            Log::get() << "Error parsing math for Y pos";
        }
    }

    return res;
}
