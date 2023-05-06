#pragma once

#include "lang/Main.hpp"
#include "lang/State.hpp"

namespace gdml {
    class GDML_DLL GDML final : public std::enable_shared_from_this<GDML> {
    protected:
        Rc<lang::Src> m_src;
        Rc<lang::SharedParser> m_parser;
        cocos2d::CCNode* m_target;
        bool m_hotReloadEnabled = false;

        void populate(cocos2d::CCNode* node) const;
        bool parse();
        void reset();

    public:
        static Rc<GDML> create();
        bool loadFile(ghc::filesystem::path const& path);
        void enableHotReload(bool enable);
        void addTo(cocos2d::CCNode* node);
        void refresh();

        Rc<lang::Src> getSrc() const;
    };

    GDML_DLL void loadGDMLFromFile(cocos2d::CCNode* node, ghc::filesystem::path const& path, bool hotReload = true);
}
