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
            m_cppData << m_varNames[node] << "->setPositionX(" << replaceExpShortHand(val) << ");\n";
        } else {
            Log::get() << "Error parsing math for X pos";
        }
    }

    auto yEdit = elem->Attribute("y");
    if (yEdit) {
        auto val = std::string(yEdit);
        if (parser.compile(val, exp)) {
            node->setPositionY(exp.value());
            m_cppData << m_varNames[node] << "->setPositionY(" << replaceExpShortHand(val) << ");\n";
        } else {
            Log::get() << "Error parsing math for Y pos";
        }
    }

    auto zEdit = elem->Attribute("z");
    if (zEdit) {
        try {
            node->setZOrder(std::stoi(zEdit));
            m_cppData << m_varNames[node] << "->setZOrder("
                << zEdit << ");\n";
        } catch(...) {}
    }

    auto scaleEdit = elem->Attribute("scale");
    if (scaleEdit) {
        try {
            auto val = std::string(scaleEdit);
            if (val._Starts_with(".")) val = "0" + val;
            node->setScale(std::stof(val));
            m_cppData << m_varNames[node] << "->setScale("
                << floatFormat(val) << ");\n";
        } catch(...) {}
    }

    auto rotateEdit = elem->Attribute("rotate");
    if (rotateEdit) {
        try {
            auto val = std::string(rotateEdit);
            if (val._Starts_with(".")) val = "0" + val;
            node->setRotation(std::stof(val));
            m_cppData << m_varNames[node] << "->setRotation("
                << floatFormat(val) << ");\n";
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
                m_cppData << m_varNames[node] << "->setContentSize({ "
                    << floatFormat(width) << ", " << floatFormat(height) << " });\n";
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
                rgba->setOpacity(static_cast<GLubyte>(std::stoi(opacityEdit)));
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
                label->setString(textEdit);
                m_cppData << m_varNames[node] << "->setString(\""
                    << textEdit << "\");\n";
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
