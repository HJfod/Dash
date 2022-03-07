#include "HotNodeManager.hpp"
#include <WackyGeodeMacros.hpp>

using namespace gdml;

HotNodeManager* HotNodeManager::get() {
    static auto inst = new HotNodeManager();
    return inst;
}

void HotNodeManager::touch(CCNode* node, bool recursive) {
    Edit edit;

    edit.pos = node->getPosition();
    edit.contentSize = node->getContentSize();
    edit.scale = node->getScale();
    edit.scaleX = node->getScaleX();
    edit.scaleY = node->getScaleY();
    edit.rotation = node->getRotation();
    edit.rotationX = node->getRotationX();
    edit.rotationY = node->getRotationY();
    edit.skewX = node->getSkewX();
    edit.skewY = node->getSkewY();
    edit.zOrder = node->getZOrder();
    edit.anchorPoint = node->getAnchorPoint();
    
    auto rgba = dynamic_cast<CCRGBAProtocol*>(node);
    if (rgba) {
        edit.color = rgba->getColor();
        edit.opacity = rgba->getOpacity();
    }
    
    auto label = dynamic_cast<CCLabelProtocol*>(node);
    if (label) {
        edit.text = label->getString();
    }

    node->retain();
    m_edits.insert({ node, edit });

    if (recursive) {
        CCARRAY_FOREACH_B_TYPE(node->getChildren(), child, CCNode) {
            this->touch(child, true);
        }
    }
}

void HotNodeManager::add(CCNode* node) {
    m_nodes.push_back(node);
}

void HotNodeManager::push(CCScene* scene) {
    m_nodes.push_back(scene);
    m_pushedScenes++;
}

void HotNodeManager::clear() {
    Log::get() << "Clearing old edits";
    for (auto& [node, edit] : m_edits) {
        node->setPosition(edit.pos);
        node->setContentSize(edit.contentSize);
        node->setRotation(edit.rotation);
        node->setRotationX(edit.rotationX);
        node->setRotationY(edit.rotationY);
        node->setScale(edit.scale);
        node->setScaleX(edit.scaleX);
        node->setScaleY(edit.scaleY);
        node->setSkewX(edit.skewX);
        node->setSkewY(edit.skewY);
        node->setZOrder(edit.zOrder);
        node->setAnchorPoint(edit.anchorPoint);

        auto rgba = dynamic_cast<CCRGBAProtocol*>(node);
        if (rgba) {
            rgba->setColor(edit.color);
            rgba->setOpacity(edit.opacity);
        }

        auto label = dynamic_cast<CCLabelProtocol*>(node);
        if (label) {
            label->setString(edit.text.c_str());
        }

        node->release();
    }
    for (auto it = m_nodes.rbegin(); it != m_nodes.rend(); it++) {
        auto node = *it;
        node->removeFromParent();
    }
    for (size_t i = 0; i < m_pushedScenes; i++) {
        CCDirector::sharedDirector()->popScene();
    }
    m_edits.clear();
    m_nodes.clear();
    m_pushedScenes = 0;
}
