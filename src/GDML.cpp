#include <GDML.hpp>
#include <lang/State.hpp>

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
    m_src = nullptr;
    m_target = nullptr;
    m_state = State::create();
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
    m_state = State::create();
    try {
        return m_state->parse(m_src).isOk();
    }
    catch(std::exception& e) {
        m_state->log(Message {
            .level = Level::Error,
            .src = m_src,
            .info = e.what(),
            .range = Range(m_src->getLocation(0)),
        });
        return false;
    }
}

void Parser::enableHotReload(bool enable) {
    m_hotReloadEnabled = enable;
}

void Parser::populate(CCNode* node) const {
    m_state->dispatchLogs();
    if (!m_state->getErrors().empty()) {
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
