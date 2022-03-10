#include "GDML.hpp"
#include "utils/exprtk.hpp"

using namespace gdml;

Result<float> GDML::processMath(CCNode* node, CCNode* parent, std::string const& text) {
    auto winSize = CCDirector::sharedDirector()->getWinSize();

    auto rgba = dynamic_cast<CCRGBAProtocol*>(node);
    auto label = dynamic_cast<CCLabelProtocol*>(node);
    auto sprite = dynamic_cast<CCSprite*>(node);

    exprtk::symbol_table<float> table;
    table.add_constant("ww", winSize.width);
    table.add_constant("wh", winSize.height);
    if (node) {
        table.add_constant("x", node->getPositionX());
        table.add_constant("y", node->getPositionY());
        table.add_constant("w", node->getContentSize().width);
        table.add_constant("h", node->getContentSize().height);
        table.add_constant("sw", node->getScaledContentSize().width);
        table.add_constant("sh", node->getScaledContentSize().height);
    }
    // for (auto& [var, val] : m_variables) {
    //     try {
    //         if (val.is_arithmetic()) {
    //             table.add_constant(var, std::stof(fmt::vformat("{" + var + "}", val)));
    //         }
    //     } catch(...) {}
    // }
    if (parent) {
        table.add_constant("px", parent->getPositionX());
        table.add_constant("py", parent->getPositionY());
        table.add_constant("pw", parent->getContentSize().width);
        table.add_constant("ph", parent->getContentSize().height);
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

    if (parser.compile(text, exp)) {
        return Ok<>(exp.value());
    }
    return Err<>("Unable to parse math expression: " + parser.error());
}
