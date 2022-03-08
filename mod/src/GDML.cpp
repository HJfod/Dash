#include <GDML.hpp>

using namespace std::string_literals;
using namespace gdml;

GDML::GDML() {
    this->registerNode("CCLabelBMFont", [](CreateData const& data) -> Result<CCNode*> {
        
    });
}

GDML* GDML::get() {
    static auto inst = new GDML;
    return inst;
}

Result<> GDML::registerAliasForNode(std::string const& tag, std::string const& other) {
    if (m_nodes.count(tag)) {
        m_nodes[other] = m_nodes[tag];
        return Ok<>();
    }
    return Err<>("Tag <" + tag + "> not found");
}

void GDML::replaceVariables(std::string& text) {
    fmt::dynamic_format_arg_store<fmt::format_context> args;
    for (auto& [name, val] : m_variables) {
        args.push_back(fmt::arg(name.c_str(), val));
    }
    text = fmt::vformat(text, args);
}

std::string GDML::process(std::string const& text) {
    auto s = text;
    this->replaceVariables(s);
    return s;
}

std::string GDML::getTextValue(tinyxml2::XMLElement* child) {
    auto text = child->GetText();
    if (!text) return "";
    return this->process(text);
}

bool GDML::hasAttribute(tinyxml2::XMLElement* child, std::string const& attr) {
    return child->Attribute(attr.c_str());
}

std::string GDML::getAttribute(
    tinyxml2::XMLElement* child,
    std::string const& attr,
    std::string const& def
) {
    auto raw = child->Attribute(attr.c_str());
    if (!raw) return def;
    return this->process(raw);
}

Result<> GDML::parseEdit(tinyxml2::XMLElement* child, CCNode* parent) {
}

Result<GDML::ParsedNodes> GDML::parseRecursive(tinyxml2::XMLElement* elem, CCNode* parent) {
    ParsedNodes result;
    for (auto child = elem->FirstChildElement(); child; child = child->NextSiblingElement()) {
        std::string name = child->Name();
        std::string subtype = "";
        if (string_utils::contains(name, ":")) {
            subtype = name.substr(name.find(":") + 1);
            name = name.substr(0, name.find(":"));
        }
        if (this->m_nodes.count(name)) {
            auto res = this->m_nodes.at(name)({ subtype, child, parent });
            if (!res) {
                return Err<>(res.error());
            }
            result.push_back(res.value());
        } else {
            return Err<>("Unknown tag <" + name + ">");
        }
    }
    return Ok<ParsedNodes>(result);
}

Result<> GDML::registerCallback(std::string const& id, SEL_MenuHandler handler, bool override) {
    if (m_callbacks.count(id) && !override) {
        return Err<>("Callback \"" + id + "\" already defined");
    }
    m_callbacks[id] = handler;
    return Ok<>();
}

SEL_MenuHandler GDML::getCallback(std::string const& id) const {
    if (!m_callbacks.count(id)) return nullptr;
    return m_callbacks.at(id);
}

void GDML::clearCallbacks() {
    m_callbacks.clear();
}

Result<> GDML::registerNode(std::string const& tag, NodeCreateFunc func, bool override) {
    if (m_nodes.count(tag) && !override) {
        return Err<>("Tag <" + tag + "> is already defined");
    }
    m_nodes[tag] = func;
    return Ok<>();
}

Result<GDML::ParsedNodes> GDML::parse(tinyxml2::XMLDocument* doc, ParseOptions const& options) {
    auto gdml = doc->FirstChildElement("gdml");
    if (gdml) {
        auto res = this->parseRecursive(gdml, options.m_parent);
        return res;
    }
}

Result<GDML::ParsedNodes> GDML::parse(std::string const& data, ParseOptions const& options) {
    tinyxml2::XMLDocument doc;
    auto r = doc.Parse(data.c_str(), data.size());
    if (r != tinyxml2::XML_SUCCESS) {
        return Err<>("Parse error: "s + parseErrorAsString(r) + " (" + std::to_string(r) + ")");
    }
    return this->parse(&doc, options);
}

Result<GDML::ParsedNodes> GDML::parseFile(ghc::filesystem::path const& path, ParseOptions const& options) {
    auto read = file_utils::readString(path);
    if (!read) {
        return Err<>(read.error());
    }
    if (!read.value().size()) return Ok<>();
    return this->parse(read.value());
}

const char* GDML::parseErrorAsString(tinyxml2::XMLError error) {
    switch (error) {
        case tinyxml2::XML_SUCCESS: return "XML_SUCCESS";
        case tinyxml2::XML_NO_ATTRIBUTE: return "XML_NO_ATTRIBUTE";
        case tinyxml2::XML_WRONG_ATTRIBUTE_TYPE: return "XML_WRONG_ATTRIBUTE_TYPE";
        case tinyxml2::XML_ERROR_FILE_NOT_FOUND: return "XML_ERROR_FILE_NOT_FOUND";
        case tinyxml2::XML_ERROR_FILE_COULD_NOT_BE_OPENED: return "XML_ERROR_FILE_COULD_NOT_BE_OPENED";
        case tinyxml2::XML_ERROR_FILE_READ_ERROR: return "XML_ERROR_FILE_READ_ERROR";
        case tinyxml2::XML_ERROR_ELEMENT_MISMATCH: return "XML_ERROR_ELEMENT_MISMATCH";
        case tinyxml2::XML_ERROR_PARSING_ELEMENT: return "XML_ERROR_PARSING_ELEMENT";
        case tinyxml2::XML_ERROR_PARSING_ATTRIBUTE: return "XML_ERROR_PARSING_ATTRIBUTE";
        case tinyxml2::XML_ERROR_IDENTIFYING_TAG: return "XML_ERROR_IDENTIFYING_TAG";
        case tinyxml2::XML_ERROR_PARSING_TEXT: return "XML_ERROR_PARSING_TEXT";
        case tinyxml2::XML_ERROR_PARSING_CDATA: return "XML_ERROR_PARSING_CDATA";
        case tinyxml2::XML_ERROR_PARSING_COMMENT: return "XML_ERROR_PARSING_COMMENT";
        case tinyxml2::XML_ERROR_PARSING_DECLARATION: return "XML_ERROR_PARSING_DECLARATION";
        case tinyxml2::XML_ERROR_PARSING_UNKNOWN: return "XML_ERROR_PARSING_UNKNOWN";
        case tinyxml2::XML_ERROR_EMPTY_DOCUMENT: return "XML_ERROR_EMPTY_DOCUMENT";
        case tinyxml2::XML_ERROR_MISMATCHED_ELEMENT: return "XML_ERROR_MISMATCHED_ELEMENT";
        case tinyxml2::XML_ERROR_PARSING: return "XML_ERROR_PARSING";
        case tinyxml2::XML_CAN_NOT_CONVERT_TEXT: return "XML_CAN_NOT_CONVERT_TEXT";
        case tinyxml2::XML_NO_TEXT_NODE: return "XML_NO_TEXT_NODE";
    }
    return "Unknown";
}
