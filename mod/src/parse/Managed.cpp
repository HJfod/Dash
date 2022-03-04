#include "Managed.hpp"
#include <WackyGeodeMacros.hpp>

Managed* Managed::get() {
    static auto inst = new Managed;
    return inst;
}

void Managed::touch(CCNode* node, bool recursive) {
    Edit edit;

    edit.pos = node->getPosition();
    edit.contentSize = node->getContentSize();
    edit.rotation = node->getRotation();
    edit.scale = node->getScale();
    
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

void Managed::add(CCNode* node) {
    m_nodes.push_back(node);
}

void Managed::clear() {
    Log::get() << "Clearing old edits";
    for (auto& [node, edit] : m_edits) {
        node->setPosition(edit.pos);
        node->setContentSize(edit.contentSize);
        node->setRotation(edit.rotation);
        node->setScale(edit.scale);

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
    m_edits.clear();
    m_nodes.clear();
}
