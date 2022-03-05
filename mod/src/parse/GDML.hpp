#pragma once

#include "shared.hpp"
#include <sstream>

static constexpr const float g_none = -99999.f;

class GDML {
protected:
    std::unordered_map<CCNode*, std::string> m_varNames;
    std::stringstream m_cppData = {};

public:
    static GDML* get();

    void acquireVarName(tinyxml2::XMLElement* elem, CCNode* node);
    Result<> handleEdit(tinyxml2::XMLElement* elem, CCNode* node);
    CCPoint handleLayoutEdit(tinyxml2::XMLElement* elem, CCNode* node);
    Result<CCNode*> createNode(tinyxml2::XMLElement* child, CCNode* node);
    Result<std::vector<CCNode*>> createLayout(tinyxml2::XMLElement* child, CCNode* node);
    Result<std::vector<CCNode*>> parseRecursive(tinyxml2::XMLElement* elem, CCNode* node);
    Result<> parse(tinyxml2::XMLDocument* doc);
    Result<> parse(ghc::filesystem::path const& path);
};
