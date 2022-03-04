#pragma once

#include "shared.hpp"

class GDML {
protected:
    std::unordered_map<std::string, tinyxml2::XMLElement*> m_macros;

public:
    static GDML* get();

    Result<> handleEdit(tinyxml2::XMLElement* elem, cocos2d::CCNode* node);
    Result<CCNode*> createNode(tinyxml2::XMLElement* child, cocos2d::CCNode* node);
    Result<> parseRecursive(tinyxml2::XMLElement* elem, cocos2d::CCNode* node);
    Result<> parse(tinyxml2::XMLDocument* doc);
    Result<> parse(ghc::filesystem::path const& path);
};
