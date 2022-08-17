#pragma once

#include <iostream>
#include <functional>

namespace gdml {
    class IO;

    namespace io {
        enum class Color {
            White,
            Black,
            Red,
            Green,
            Blue,
            Yellow,
            Purple,
            Gray,
            Light,
            Lime,
            Lemon,
            Pink,
            Cyan,
        };
        struct BG {
            Color color;
            operator Color() {
                return color;
            }
        };

        using ColorSetter = std::function<void(IO&, Color)>;
        
        extern const ColorSetter DEFAULT_COLOR_SETTER;
        extern const ColorSetter DEFAULT_BG_SETTER;
    }

    class IO {
    protected:
        std::ostream& m_cout;
        std::istream& m_cin;
        io::ColorSetter m_colorSetter;
        io::ColorSetter m_bgSetter;
        io::Color m_color;
        io::Color m_bg;

        void setup();

    public:
        inline IO(
            std::ostream& cout = std::cout,
            std::istream& cin = std::cin,
            io::ColorSetter colorSetter = io::DEFAULT_COLOR_SETTER,
            io::ColorSetter bgSetter = io::DEFAULT_BG_SETTER
        ) : m_cout(cout),
            m_cin(cin),
            m_colorSetter(colorSetter),
            m_bgSetter(bgSetter)
        {
            m_color = io::Color::White;
            m_bg = io::Color::Black;

            this->setup();
        }
        inline ~IO() {
            this->color(io::Color::White);
            this->bg(io::Color::Black);
        }
        
        inline IO& color(io::Color color) {
            if (m_colorSetter) m_colorSetter(*this, color);
            m_color = color;
            return *this;
        }
        inline IO& bg(io::Color color) {
            if (m_bgSetter) m_bgSetter(*this, color);
            m_bg = color;
            return *this;
        }
        inline io::Color const& color() const {
            return m_color;
        }
        inline io::Color const& bg() const {
            return m_bg;
        }

        template<typename T>
        IO& operator<<(T value) {
            m_cout << value;
            return *this;
        }
        template<typename T>
        IO& operator>>(T& value) {
            m_cin >> value;
            return *this;
        }
        template<>
        IO& operator<< <io::Color>(io::Color color) {
            return this->color(color);
        }
        template<>
        IO& operator<< <io::BG>(io::BG color) {
            return this->bg(color.color);
        }
        IO& operator<<(IO& value) {
            return value;
        }
    };

    extern IO DEFAULT_IO;
}
