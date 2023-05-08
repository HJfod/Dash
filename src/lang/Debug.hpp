#pragma once

#include <lang/Token.hpp>
#include <lang/Expr.hpp>

using namespace geode::prelude;
using namespace gdml;
using namespace gdml::lang;

template <class T>
std::string debugPrint(T const& t, size_t indent) {
    static_assert(!std::is_same_v<T, T>, "debugPrint not defined for type");
}

template <class T>
    requires requires {
        TokenTraits<T>::TYPE_NAME;
    }
std::string debugPrint(T const& t, size_t) {
    return tokenToString(t, true);
}

template <class E>
    requires std::is_base_of_v<Expr, E>
std::string debugPrint(Rc<E> const& expr, size_t indent) {
    return expr->debug(indent);
}

template <class T>
    requires requires(T const& t) {
        debugPrint(t, 0);
    }
std::string debugPrint(Option<T> const& opt, size_t i) {
    if (opt) {
        return fmt::format("Some({})", debugPrint(opt.value(), i));
    }
    else {
        return "None";
    }
}

template <class T>
    requires requires(T const& t) {
        debugPrint(t, 0);
    }
std::string debugPrint(Vec<T> const& vec, size_t i) {
    if (vec.empty()) {
        return "[]";
    }
    std::string ret = "[\n";
    bool first = true;
    for (auto& t : vec) {
        if (!first) {
            ret += ",\n";
        }
        first = false;
        ret += std::string(i + 4, ' ') + debugPrint(t, i + 4);
    }
    ret += "\n" + std::string(i, ' ') + "]";
    return ret;
}

template <>
inline std::string debugPrint(bool const& b, size_t) {
    return b ? "true" : "false";
}

template <>
inline std::string debugPrint(IdentPath const& p, size_t) {
    return p.toString();
}

template <>
inline std::string debugPrint(Param const& p, size_t indent) {
    std::string ret = "{\n";
    ret += std::string(indent + 4, ' ') + "name: " + debugPrint(p.name, indent + 4) + "\n";
    ret += std::string(indent + 4, ' ') + "type: " + debugPrint(p.type, indent + 4) + "\n";
    ret += std::string(indent + 4, ' ') + "value: " + debugPrint(p.value, indent + 4) + "\n";
    ret += std::string(indent, ' ') + "}";
    return ret;
}

struct DebugPrint {
    size_t m_indent;
    std::string m_class;
    std::vector<std::pair<std::string, std::string>> m_members;

    DebugPrint(std::string const& className, size_t i)
      : m_class(className), m_indent(i) {}
    DebugPrint(DebugPrint const&) = delete;
    DebugPrint(DebugPrint&&) = delete;

    template <class T>
    DebugPrint& member(std::string const& name, T const& value) {
        m_members.emplace_back(name, debugPrint(value, m_indent + 4));
        return *this;
    }

    operator std::string() const {
        auto ret = m_class + " {\n";
        for (auto& mem : m_members) {
            ret += std::string(m_indent + 4, ' ') + mem.first + ": " + mem.second + "\n";
        }
        ret += std::string(m_indent, ' ') + "}";
        return ret;
    }
};

template <>
class fmt::formatter<gdml::lang::IdentPath> {
public:
    constexpr auto parse (format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (gdml::lang::IdentPath const& path, Context& ctx) const {
        return format_to(ctx.out(), "{}", path.toString());
    }
};

template <>
class fmt::formatter<std::source_location> {
public:
    constexpr auto parse (format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (std::source_location const& loc, Context& ctx) const {
        return format_to(ctx.out(), "{}::{}:{} in {}", loc.function_name(), loc.line(), loc.column(), loc.file_name());
    }
};
