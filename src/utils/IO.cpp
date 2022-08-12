#include "IO.hpp"
#include <unordered_map>

using namespace gdml;
using namespace gdml::io;

#ifdef _WIN32

#include <Windows.h>

static HANDLE CONSOLE_HANDLE = GetStdHandle(STD_OUTPUT_HANDLE);
static std::unordered_map<Color, char> COLOR_BITS {
    { Color::White,  0b1110 },
    { Color::Black,  0b0000 },
    { Color::Red,    0b1000 },
    { Color::Green,  0b0100 },
    { Color::Blue,   0b0010 },
    { Color::Yellow, 0b1100 },
    { Color::Purple, 0b1010 },
    { Color::Gray,   0b0001 },
    { Color::Light,  0b1111 },
    { Color::Lime,   0b0101 },
    { Color::Lemon,  0b1101 },
    { Color::Pink,   0b1011 },
    { Color::Cyan,   0b0111 },
};

static WORD generate(Color fg, Color bg) {
    WORD res = 0;

    auto fgb = COLOR_BITS.at(fg);
    auto bgb = COLOR_BITS.at(bg);

    if (fgb & 0b1000) res |= FOREGROUND_RED;
    if (fgb & 0b0100) res |= FOREGROUND_GREEN;
    if (fgb & 0b0010) res |= FOREGROUND_BLUE;
    if (fgb & 0b0001) res |= FOREGROUND_INTENSITY;

    if (bgb & 0b1000) res |= BACKGROUND_RED;
    if (bgb & 0b0100) res |= BACKGROUND_GREEN;
    if (bgb & 0b0010) res |= BACKGROUND_BLUE;
    if (bgb & 0b0001) res |= BACKGROUND_INTENSITY;

    return res;
}

const ColorSetter io::DEFAULT_COLOR_SETTER = [](IO& io, Color color) {
    SetConsoleTextAttribute(CONSOLE_HANDLE, generate(color, io.bg()));
};

const ColorSetter io::DEFAULT_BG_SETTER = [](IO& io, Color bg) {
    SetConsoleTextAttribute(CONSOLE_HANDLE, generate(io.color(), bg));
};

IO gdml::DEFAULT_IO = IO();

#endif
