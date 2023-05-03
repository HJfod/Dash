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
        m_parser->getSrc()->onChanged([=](auto) {
            m_parser->refresh();
        });

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

void Parser::reset() {
    m_errors.clear();
    m_src = nullptr;
    m_ast = nullptr;
    m_target = nullptr;
}

bool Parser::loadFile(ghc::filesystem::path const& path) {
    this->reset();
    auto srcRes = SrcFile::from(path);
    if (!srcRes) {
        return false;
    }
    m_src = srcRes.unwrap();
    return this->parse();
}

bool Parser::parse() {
    m_ast = nullptr;
    auto stream = m_src->read();
    try {
        auto ast = AST::pull(stream);
        if (!ast) {
            for (auto& err : stream.errors()) {
                m_errors.push_back(err.toString());
            }
        }
        else {
            log::debug("Succesfully parsed AST");
            m_ast = ast.unwrap();
            log::debug("{}", m_ast->debug());
        }
        return ast.isOk();
    }
    catch(std::exception& e) {
        m_errors.push_back(e.what());
        return false;
    }
}

void Parser::enableHotReload(bool enable) {
    m_hotReloadEnabled = enable;
}

void Parser::populate(CCNode* node) const {
    if (!m_errors.empty()) {
        for (auto& err : m_errors) {
            log::error("{}", err);
        }
        node->addChild(CCLabelBMFont::create(
            "There were errors loading GDML - see console",
            "bigFont.fnt"
        ));
    }
}

void Parser::addTo(CCNode* node) {
    if (m_hotReloadEnabled) {
        m_target = node;
        node->addChild(HotReloadNode::create(shared_from_this()));
    }
    this->populate(node);
}

void Parser::refresh() {
    if (!m_target) return;
    m_errors.clear();
    this->parse();
    this->populate(m_target);
}

Rc<lang::Src> Parser::getSrc() const {
    return m_src;
}

void gdml::loadGDMLFromFile(CCNode* node, ghc::filesystem::path const& path, bool hotReload) {
    auto parser = Parser::create();
    parser->enableHotReload(hotReload);
    parser->loadFile(path);
    parser->addTo(node);
}
