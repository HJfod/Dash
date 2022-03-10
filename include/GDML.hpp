#pragma once

#include "shared.hpp"
#include <sstream>
#include <any>
#include <fmt/include/fmt/format.h>
#include <fmt/include/fmt/args.h>

namespace gdml {
    class GDML_DLL GDML {
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
        using NodeCreateFunc = std::function<Result<CCNode*>(CreateData const&)>;
        using FallbackCreateFunc = std::function<CCNode*(CreateData const&)>;
        using ParsedNodes = std::vector<CCNode*>;

        struct ParseOptions {
            CCNode* m_parent = nullptr;
            bool m_hotReload = false;
        };
        struct Metadata {
            std::string m_name = "";
        };

    protected:
        std::unordered_map<std::string, SEL_MenuHandler> m_callbacks;
        std::unordered_map<std::string, NodeCreateFunc> m_nodes;
        std::unordered_map<std::string, FallbackCreateFunc> m_fallbackNodes;
        fmt::dynamic_format_arg_store<fmt::format_context> m_variables;
        std::unordered_map<CCNode*, Metadata> m_metadata;
        std::unordered_map<std::string, CCNode*> m_nodeNames;
        std::vector<CCNode*> m_created;
        ParseOptions m_options;

        Result<CCNode*> createNode(tinyxml2::XMLElement* elem, CCNode* parent);
        Result<ParsedNodes> parseRecursive(tinyxml2::XMLElement* child, CCNode* parent);
        Result<> parseEdit(tinyxml2::XMLElement* child, CCNode* node, CCNode* parent);

        Result<std::string> replaceVariables(std::string const& text);
        Result<std::string> process(std::string const& text);
        Result<float> processMath(CCNode* node, CCNode* parent, std::string const& exp);
        Result<std::string> getTextValue(
            tinyxml2::XMLElement* child,
            std::string const& default = ""
        );
        bool hasAttribute(
            tinyxml2::XMLElement* child,
            std::string const& attr
        );
        Result<std::string> getAttribute(
            tinyxml2::XMLElement* child,
            std::string const& attr,
            std::string const& default = ""
        );
        Result<float> getAttributeCalced(
            tinyxml2::XMLElement* child,
            CCNode* node,
            CCNode* parent,
            std::string const& attr,
            float default = .0f
        );
        Result<> registerAliasForNode(
            std::string const& newTag,
            std::string const& oldTag
        );
        void clear();

        GDML();

    public:
        static GDML* get();

        Result<> [[nodiscard]] registerCallback(
            std::string const& id,
            SEL_MenuHandler handler,
            bool override = false
        );
        SEL_MenuHandler getCallback(std::string const& id) const;
        Result<> [[nodiscard]] registerNode(
            std::string const& tag,
            NodeCreateFunc func,
            FallbackCreateFunc fallbackFunc,
            bool override = false
        );
        Result<> [[nodiscard]] registerNode(
            std::string const& tag,
            NodeCreateFunc func,
            bool override = false
        );
        void clearCallbacks();

        CCNode* getNodeByName(std::string const& name) const;

        static const char* parseErrorAsString(tinyxml2::XMLError error);

        Result<ParsedNodes> parseDoc(
            tinyxml2::XMLDocument* doc,
            ParseOptions const& options = ParseOptions()
        );
        Result<ParsedNodes> parseString(
            std::string const& data, 
            ParseOptions const& options = ParseOptions()
        );
        Result<ParsedNodes> parseFile(
            ghc::filesystem::path const& path,
            ParseOptions const& options = ParseOptions()
        );
        static Result<ParsedNodes> parse(
            std::string const& data, 
            ParseOptions const& options = ParseOptions()
        );
    };

    Result<GDML::ParsedNodes> GDML_DLL operator"" _gdml(const char* str, size_t);

    #define GDML_REGISTER_CALLBACK(cb) GDML::get()->registerCallback(#cb, menu_selector(cb))
    #define GDML_REGISTER(node) \
        auto reg##node = GDML::get()->registerNode(#node, \
            [](auto) { return Ok<>(node::create()); })
}
