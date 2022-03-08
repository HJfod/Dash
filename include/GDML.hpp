#pragma once

#include "shared.hpp"
#include <sstream>
#include <any>
#include <fmt/include/fmt/format.h>
#include <fmt/include/fmt/args.h>

namespace gdml {
    class GDML {
    public:
        struct CreateData {
            std::string m_subType;
            tinyxml2::XMLElement* m_element;
            CCNode* m_parent = nullptr;

            CreateData() = default;
            inline CreateData(
                std::string const& subType,
                tinyxml2::XMLElement* element,
                CCNode* parent
            ) : m_subType(subType),
                m_element(element),
                m_parent(parent) {}
        };
        struct ParseOptions {
            CCNode* m_parent = nullptr;
        };
        using NodeCreateFunc = std::function<Result<CCNode*>(CreateData const&)>;
        using ParsedNodes = std::vector<CCNode*>;

    protected:
        std::unordered_map<std::string, SEL_MenuHandler> m_callbacks;
        std::unordered_map<std::string, NodeCreateFunc> m_nodes;
        std::unordered_map<std::string, fmt::basic_format_arg<fmt::format_context>> m_variables;

        Result<ParsedNodes> parseRecursive(tinyxml2::XMLElement* child, CCNode* parent);
        Result<> parseEdit(tinyxml2::XMLElement* child, CCNode* parent);

        void replaceVariables(std::string& text);
        std::string process(std::string const& text);
        Result<float> processMath(CCNode* node, std::string const& exp);
        std::string getTextValue(tinyxml2::XMLElement* child);
        bool hasAttribute(tinyxml2::XMLElement* child, std::string const& attr);
        std::string getAttribute(
            tinyxml2::XMLElement* child,
            std::string const& attr,
            std::string const& default = ""
        );
        Result<> registerAliasForNode(std::string const& tag, std::string const& other);

        GDML();

    public:
        static GDML* get();

        Result<> registerCallback(std::string const& id, SEL_MenuHandler handler, bool override = false);
        SEL_MenuHandler getCallback(std::string const& id) const;
        Result<> registerNode(std::string const& tag, NodeCreateFunc func, bool override = false);
        void clearCallbacks();

        static const char* parseErrorAsString(tinyxml2::XMLError error);

        Result<ParsedNodes> parse(
            tinyxml2::XMLDocument* doc,
            ParseOptions const& options = ParseOptions()
        );
        Result<ParsedNodes> parse(
            std::string const& data, 
            ParseOptions const& options = ParseOptions()
        );
        Result<ParsedNodes> parseFile(
            ghc::filesystem::path const& path,
            ParseOptions const& options = ParseOptions()
        );
    };

    #define REGISTER_CALLBACK(cb) GDML::get()->registerCallback(#cb, menu_selector(cb))
}
