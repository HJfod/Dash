#include <GDML.hpp>

using namespace gdml;
using namespace gdml::lang;
using namespace geode::prelude;

class HotReloadNode : public CCNode {
protected:
    Rc<Parser> m_parser;

    bool init(Rc<Parser> parser) {
        if (!CCNode::init())
            return false;
        
        m_parser = parser;

        return true;
    }

public:
    static HotReloadNode* create(Rc<Parser> parser) {
        auto ret = new HotReloadNode();
        if (ret && ret->init(parser)) {
            ret->autorelease();
            return ret;
        }
        CC_SAFE_DELETE(ret);
        return nullptr;
    }
};

Rc<Parser> Parser::create() {
    return std::make_shared<Parser>();
}

Result<> Parser::loadFile(ghc::filesystem::path const& path) {
    GEODE_UNWRAP_INTO(auto file, SrcFile::from(path));
    auto stream = file->read();
    GEODE_UNWRAP_INTO(m_ast, Expr::pull(stream).expect("Unable to parse AST: {error}"));
    return Ok();
}

void Parser::enableHotReload(bool enable) {
    m_hotReloadEnabled = enable;
}

void Parser::addTo(CCNode* node) {
    if (m_hotReloadEnabled) {
        node->addChild(HotReloadNode::create(shared_from_this()));
    }
}

void gdml::loadGDMLFromFile(CCNode* node, ghc::filesystem::path const& path, bool hotReload) {
    auto parser = Parser::create();
    parser->enableHotReload(hotReload);
    parser->loadFile(path);
    parser->addTo(node);
}
