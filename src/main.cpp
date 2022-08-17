#include <iostream>
#include "utils/about.hpp"
#include "compiler/GDML.hpp"
#include <chrono>
#include "cli/CLI.hpp"

using namespace gdml;
using namespace gdml::io;

using Clock = std::chrono::high_resolution_clock;
using TimePoint = std::chrono::time_point<Clock>;
using Time = std::chrono::milliseconds;
using std::chrono::duration_cast;

#define INFO()  Color::Cyan  << "[info] " << Color::White
#define ERROR() Color::Red   << "[fail] " << Color::White
#define SUCC()  Color::Lime  << "[done] " << Color::White
#define WARN()  Color::Yellow<< "[warn] " << Color::White

constexpr const char* TIME_LITERAL = "ms";

int main(int argc, char* argv[]) {
    CLI cli {
        { "debug", None },
        { "intellisense", None },
        { "silent", None },
        { "dry", None },
    };

    IO& io = DEFAULT_IO;

    auto cliRes = cli.parse(argc, argv);
    if (!cliRes) {
        io << Color::Red << cliRes.unwrapErr() << "\n";
        return 1;
    }

    auto flags = DEFAULT_FLAGS;
    if (cli.getFlag("debug")) {
        flags = DEBUG_FLAGS;
    }
    if (cli.getFlag("silent")) {
        flags = Flags::None;
    }
    if (cli.getFlag("dry")) {
        flags |= Flags::DryRun;
    }

    GDML compiler(flags);
    for (auto arg : cli.getArgs()) {
        std::string input = arg;
        std::string output = arg + ".cpp";

        auto arrow = arg.find("->");
        if (arrow != std::string::npos) {
            input = arg.substr(0, arrow);
            output = arg.substr(arrow + 2);
        }

        io << INFO() << "Transpiling " << input << "\n";

        auto startTime = Clock::now();
        auto res = compiler.compileFile(input, output);

        io
            << INFO()
            << "Transpiling done in "
            << duration_cast<Time>(Clock::now() - startTime).count() << TIME_LITERAL
            << ": " << Color::Lime << errorToString(res) << "\n";

        if (res != Error::OK) {
            io << ERROR() << "Some transpilations failed :(\n";
            return 2;
        }
    }
    io << SUCC() << "Transpilation finished\n";

    return 0;
}
