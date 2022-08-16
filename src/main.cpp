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

constexpr const char* TIME_LITERAL = "ms";

int main(int argc, char* argv[]) {
    CLI cli {
        { "debug", None },
        { "intellisense", None },
    };

    IO& io = DEFAULT_IO;

    auto cliRes = cli.parse(argc, argv);
    if (!cliRes) {
        io << Color::Red << cliRes.unwrapErr() << "\n";
        return 1;
    }

    GDML compiler(cli.getFlag("debug") ? DEBUG_FLAGS : DEFAULT_FLAGS);
    for (auto arg : cli.getArgs()) {
        std::string input = arg;
        std::string output = arg + ".cpp";

        auto arrow = arg.find("->");
        if (arrow != std::string::npos) {
            input = arg.substr(0, arrow);
            output = arg.substr(arrow + 2);
        }

        std::cout << "Transpiling " << input << "\n";

        auto startTime = Clock::now();
        auto res = compiler.compileFile(input, output);

        std::cout
            << "Transpiling done in "
            << duration_cast<Time>(Clock::now() - startTime).count() << TIME_LITERAL
            << ": " << errorToString(res) << "\n";

        if (res != Error::OK) {
            std::cout << "Some transpilations failed :(\n";
            return 2;
        }
    }
    std::cout << "Transpilation finished\n";

    return 0;
}
