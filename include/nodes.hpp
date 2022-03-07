#pragma once

#include "shared.hpp"

namespace gdml {
    class Layout : public CCNode {
    public:
        enum Align {
            Start,
            Middle,
            End,
        };
        static Align alignFromString(const char* str);

    protected:
        float m_padding = 0.f;
        Align m_align = Align::Middle;

    public:
        virtual void arrange() = 0;
    };

    class RowLayout : public Layout {
    public:
        virtual void arrange() override;
        static RowLayout* create();
    };

    class ColumnLayout : public Layout {
    public:
        virtual void arrange() override;
        static ColumnLayout* create();
    };
}
