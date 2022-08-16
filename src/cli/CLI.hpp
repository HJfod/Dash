#pragma once

#include <utils/Types.hpp>

namespace gdml {
    struct Flag {
        struct Value {
            std::string value;
            std::string defaultValue;
        };

        std::string flag;
        Option<Value> value;
    };

    class CLI {
    protected:
        std::vector<std::string> m_args;
        std::unordered_map<std::string, Option<Flag::Value>> m_flags;
    
    public:
        CLI(std::initializer_list<Flag> const& flags);

        std::vector<std::string> const& getArgs() const;
        bool getFlag(std::string const& flag) const;
        Flag::Value getFlagValue(std::string const& flag) const;

        Result<void, std::string> parse(int argc, char* argv[]);
    };
}
