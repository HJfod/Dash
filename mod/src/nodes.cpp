#include <nodes.hpp>
#include <WackyGeodeMacros.hpp>

using namespace gdml;

Layout::Align Layout::alignFromString(const char* str) {
    if (!str) return Layout::Align::Middle;
    switch (hash(str)) {
        case hash("start"): return Layout::Align::Start;
        case hash("middle"): return Layout::Align::Middle;
        case hash("end"): return Layout::Align::End;
        default: return Layout::Align::Middle;
    }
}

void RowLayout::arrange() {
    CCSize totalSize = { 0, 0 };
    CCARRAY_FOREACH_B_TYPE(this->getChildren(), node, CCNode) {
        totalSize += node->getScaledContentSize() + CCSize { m_padding, m_padding };
    }
    if (totalSize.width > m_padding) totalSize.width -= m_padding;
    if (totalSize.height > m_padding) totalSize.height -= m_padding;
    float alignOffset = 0.f; // same as start
    switch (m_align) {
        case Align::End: alignOffset = -totalSize.width; break;
        default:
        case Align::Middle: alignOffset = -totalSize.width / 2; break;
    }
    float caretPos = 0;
    CCARRAY_FOREACH_B_TYPE(this->getChildren(), node, CCNode) {
        auto caretInc = node->getScaledContentSize().width;
        caretPos += caretInc / 2;
        node->setPositionX(caretPos + alignOffset);
        caretPos += caretInc / 2 + m_padding;
    }
}

RowLayout* RowLayout::create() {
    auto ret = new RowLayout;
    if (ret && ret->init()) {
        ret->autorelease();
        return ret;
    }
    CC_SAFE_DELETE(ret);
    return nullptr;
}

void ColumnLayout::arrange() {
    CCSize totalSize = { 0, 0 };
    CCARRAY_FOREACH_B_TYPE(this->getChildren(), node, CCNode) {
        totalSize += node->getScaledContentSize() + CCSize { m_padding, m_padding };
    }
    if (totalSize.width > m_padding) totalSize.width -= m_padding;
    if (totalSize.height > m_padding) totalSize.height -= m_padding;
    float alignOffset = 0.f; // same as start
    switch (m_align) {
        case Align::End: alignOffset = -totalSize.height; break;
        default:
        case Align::Middle: alignOffset = -totalSize.height / 2; break;
    }
    float caretPos = 0;
    CCARRAY_FOREACH_B_TYPE(this->getChildren(), node, CCNode) {
        auto caretInc = node->getScaledContentSize().height;
        caretPos += caretInc / 2;
        node->setPositionY(caretPos + alignOffset);
        caretPos += caretInc / 2 + m_padding;
    }
}

ColumnLayout* ColumnLayout::create() {
    auto ret = new ColumnLayout;
    if (ret && ret->init()) {
        ret->autorelease();
        return ret;
    }
    CC_SAFE_DELETE(ret);
    return nullptr;
}
