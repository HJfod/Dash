#include <iostream>
#include "utils/about.hpp"
#include "compiler/GDML.hpp"

using namespace gdml;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cout << "Usage: " << PROJECT_BINARY_NAME << " file0.gdml file1.gdml ... [options]\n";
        return 1;
    }

    GDML compiler;
    for (int i = 1; i < argc; i++) {
        auto arg = std::string(argv[i]);

        std::string input = arg;
        std::string output = arg + ".cpp";

        auto arrow = arg.find("->");
        if (arrow != std::string::npos) {
            input = arg.substr(0, arrow);
            output = arg.substr(arrow + 2);
        }

        std::cout << "Transpiling " << input << "\n";
        auto res = compiler.compileFile(input, output);
        std::cout << "Transpiling done: " << errorToString(res) << "\n";

        if (res != Error::OK) {
            std::cout << "Some transpilations failed :(\n";
            return 2;
        }
    }
    std::cout << "Transpilation finished\n";

    return 0;
}
