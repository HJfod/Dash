#include "CLI.hpp"

using namespace gdml;
using namespace std::string_literals;

CLI::CLI(std::initializer_list<Flag> const& flags) {
    for (auto const& flag : flags) {
        m_flags.insert({ flag.flag, flag.value });
    }
}

std::vector<std::string> const& CLI::getArgs() const {
    return m_args;
}

bool CLI::getFlag(std::string const& flag) const {
    return m_flags.count(flag);
}

Flag::Value CLI::getFlagValue(std::string const& flag) const {
    return m_flags.at(flag).value();
}

Result<void, std::string> CLI::parse(int argc, char* argv[]) {
    auto knownFlags = m_flags;
    m_flags = {};

    std::string collectedFlag {};
    for (int i = 1; i < argc; i++) {
        auto arg = std::string(argv[i]);
        if (arg.starts_with("-")) {
            if (!arg.starts_with("--")) {
                return "Flags need to be prefixed with '--'"s;
            }
            
            if (collectedFlag.size()) {
                if (!knownFlags.count(collectedFlag)) {
                    return "Unexpected flag '--" + collectedFlag + "'";
                }
                if (knownFlags.at(collectedFlag).has_value()) {
                    return "Flag '--" + collectedFlag + "' expected to have a value";
                }
                m_flags.insert({ collectedFlag, None });
            }
            collectedFlag = arg.substr(2);

        } else {
            if (collectedFlag.size()) {
                
                m_flags.insert({
                    collectedFlag,
                    Option<Flag::Value>(Flag::Value { arg })
                });
                collectedFlag.clear();

            } else {

                m_args.push_back(arg);

            }
        }
    }
    if (collectedFlag.size()) {
        if (!knownFlags.count(collectedFlag)) {
            return "Unexpected flag '--" + collectedFlag + "'";
        }
        if (knownFlags.at(collectedFlag).has_value()) {
            return "Flag '--" + collectedFlag + "' expected to have a value";
        }
        m_flags.insert({ collectedFlag, None });
    }
    return Ok();
}

