#include <GDML.hpp>
#include <lang/State.hpp>

using namespace gdml;
using namespace gdml::lang;
using namespace geode::prelude;

class HotReloadNode : public CCNode {
protected:
    Rc<GDML> m_parser;

    bool init(Rc<GDML> parser) {
        if (!CCNode::init())
            return false;
        
        m_parser = parser;
        m_parser->getSrc()->onChanged([=](auto) {
            m_parser->refresh();
        });

        return true;
    }

public:
    static HotReloadNode* create(Rc<GDML> parser) {
        auto ret = new HotReloadNode();
        if (ret && ret->init(parser)) {
            ret->autorelease();
            return ret;
        }
        CC_SAFE_DELETE(ret);
        return nullptr;
    }
};

Rc<GDML> GDML::create() {
    return std::make_shared<GDML>();
}

void GDML::reset() {
    m_src = nullptr;
    m_target = nullptr;
    m_parser = nullptr;
}

bool GDML::loadFile(ghc::filesystem::path const& path) {
    this->reset();
    auto srcRes = SrcFile::from(path);
    if (!srcRes) {
        return false;
    }
    m_src = srcRes.unwrap();
    return this->parse();
}

bool GDML::parse() {
    return (m_parser = SharedParser::create(m_src))->compile().isOk();
}

void GDML::enableHotReload(bool enable) {
    m_hotReloadEnabled = enable;
}

void GDML::populate(CCNode* node) const {
    m_parser->dispatchLogs();
    if (!m_parser->getErrors().empty()) {
        node->addChild(CCLabelBMFont::create(
            "There were errors loading GDML - see console",
            "bigFont.fnt"
        ));
    }
}

void GDML::addTo(CCNode* node) {
    if (m_hotReloadEnabled) {
        m_target = node;
        node->addChild(HotReloadNode::create(shared_from_this()));
    }
    this->populate(node);
}

void GDML::refresh() {
    if (!m_target) return;
    this->parse();
    this->populate(m_target);
}

Rc<lang::Src> GDML::getSrc() const {
    return m_src;
}

void gdml::loadGDMLFromFile(CCNode* node, ghc::filesystem::path const& path, bool hotReload) {
    auto parser = GDML::create();
    parser->enableHotReload(hotReload);
    parser->loadFile(path);
    parser->addTo(node);
}
