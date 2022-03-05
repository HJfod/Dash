#pragma once

#include "shared.hpp"
#include <sstream>
#include <any>

static constexpr const float g_none = -99999.f;

struct Variable {
    enum Type {
        Int, Float, String,
    };
    std::any m_value;
    Type m_type;
};

class GDML {
protected:
    std::unordered_map<CCNode*, std::string> m_varNames;
    std::unordered_map<std::string, tinyxml2::XMLElement*> m_models;
    std::unordered_map<std::string, Variable> m_variables;
    std::stringstream m_cppData = {};

public:
    static GDML* get();

    std::string replaceVariables(const char* str);
    std::string replaceVariables(std::string const& str);
    void acquireVarName(tinyxml2::XMLElement* elem, CCNode* node);
    Result<> handleEdit(tinyxml2::XMLElement* elem, CCNode* node);
    CCPoint handleLayoutEdit(tinyxml2::XMLElement* elem, CCNode* node);
    Result<CCNode*> createNode(tinyxml2::XMLElement* child, CCNode* node);
    Result<std::vector<CCNode*>> createLayout(tinyxml2::XMLElement* child, CCNode* node);
    Result<std::vector<CCNode*>> createCustom(tinyxml2::XMLElement* child, CCNode* node);
    Result<std::vector<CCNode*>> createVariable(tinyxml2::XMLElement* child, CCNode* node);
    Result<std::vector<CCNode*>> parseRecursive(tinyxml2::XMLElement* elem, CCNode* node);
    Result<> parse(tinyxml2::XMLDocument* doc);
    Result<> parse(ghc::filesystem::path const& path);
};
